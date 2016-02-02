{-# LANGUAGE DeriveDataTypeable #-}

module Kyarigwo.AddModule where

import           ClassyPrelude
import           Data.Char              hiding (Space)
import           Data.Data
import           Data.Generics          ()
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as N
import qualified Data.Text              as T
import           System.Directory
import           System.FilePath
import           Text.Hastache
import           Text.Hastache.Context
import           Text.Regex.Applicative

-- | The arguments passed to the program
data Args = Args FilePath Text
  -- ^ Args templateFilePath moduleFullName

-- | Entry point to program
addModule :: Args -> IO ()
addModule (Args templatePath moduleFullName) = do
  let moduleComponents = N.nonEmpty (T.split (== '.') moduleFullName)
                             `orError` "Error: the module name must not be empty"
  templateFile <- readFile templatePath
  files <- createModuleFiles moduleComponents templateFile
  mapM_ writeCreatedFile files

orError :: Maybe a -> Text -> a
orError m t = fromMaybe (error (unpack t)) m

-- | Describes a file that we want to create
data CreatedFile = CreatedFile FilePath Text
  -- ^ CreatedFile path to file, contents of file

-- | Takes a created file, and writes it to disk
writeCreatedFile :: CreatedFile -> IO ()
writeCreatedFile (CreatedFile path contents) = do
  let dirPath = takeDirectory path
  createDirectoryIfMissing True dirPath
  writeFile path contents

-- | Given the module name and template file contents,
--   creates a list of files to create.
--   needs to run in IO for hastache
createModuleFiles :: NonEmpty Text -- ^ the parts of a module name
                  -> Text -- ^ template file contents
                  -> IO [CreatedFile] -- ^ list of files to create
createModuleFiles moduleFullName templateContents =
  let context = splitModuleName moduleFullName
      -- generate the variables of the template from the module name
      templateList = splitTemplate templateContents
      -- split the template into file names and contents of each file pairs
  in mapM (applyTemplate context) templateList
      -- substitute the variables into each file name and contents pairs
      -- and return the results list

-- split the module name into the required variables

-- | this is the list of variables that will be substituted into the filename
--   and contents to create each file
--
--   For Example:
--
--     if creating the module Name1.Name2.Name3 the output is:
--
--     * basename = Name3
--     * fspath   = Name1/Name2
--     * modpath  = Name1.Name2
--     * modname  = Name1.Name2.Name3
data ModuleNameComponents = ModuleNameComponents {
  basename :: Text, -- ^ the name of the module without the preceding path
  fspath   :: Text, -- ^ the path to the module as a file system path
  modpath  :: Text, -- ^ the path to the module as a haskell module path
  modname  :: Text  -- ^ the full module name, as entered on the command line
  }
  deriving (Show, Eq, Data, Typeable)

-- | splits a module name into the above components
splitModuleName :: NonEmpty Text -> ModuleNameComponents
splitModuleName lst = let start = N.init lst
                       in ModuleNameComponents
                             (N.last lst)
                             (intercalate "/" start)
                             (intercalate "." start)
                             (intercalate "." (N.toList lst))

-- split the template file into pairs of the file path template and the file contents
-- template

-- | Describes the two parts of a file template -- the filename and the file contents
data TemplateParts = TemplateParts Text Text
-- ^ TemplateParts [filename template]  [file contents template]

-- | Splits the template into the required parts
--
--   The template file contains a sequence of sections beginning with a line of the form:
--
--   {-\# START_FILE filenametemplate \#-}
--
--   with filenametemplate a mustache template
splitTemplate :: Text -> [TemplateParts]
splitTemplate txt = snd $ foldr workfn ([], []) (lines txt)
  where workfn ln (lns, res) = case matchFileStart ln of
          Just pat -> ([], TemplateParts pat (unlines lns):res)
          Nothing  -> (ln:lns, res)

-- | Attempts to match argument against the start file header, and returns the
--   filenametemplate if successful
matchFileStart :: Text -> Maybe Text
matchFileStart txt = (\(_, x, _) -> x) <$> findFirstInfix headerRE (unpack txt)
  where
    headerRE = "{-#" *> spaces *> "START_FILE" *> spaces *> nonSpaces <* spaces <* "#-}"
    nonSpaces = pack <$> some (psym (not . isSpace))
    spaces = (pack <$> some (psym isSpace)) :: RE Char Text

-- applies the split module name template to the given templates

-- | Applies the templates in TemplateParts in a ModuleNameComponents context
--   to give a CreatedFile
applyTemplate :: ModuleNameComponents -> TemplateParts  -> IO CreatedFile
applyTemplate components (TemplateParts path contents) =
  let hFn = doHastache $ mkGenericContext components
      doHastache context txt = toStrict <$> hastacheStr defaultConfig txt context
  in CreatedFile <$> (unpack <$> hFn path) <*> hFn contents
