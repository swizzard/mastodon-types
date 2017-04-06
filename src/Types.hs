{-# LANGUAGE TemplateHaskell #-}
module Web.Mastodon.Types
    (
      -- * Top-level types
        Account(..)
      , Application(..)
      , Attachment(..)
      , Card(..)
      , Context(..)
      , Error(..)
      , Instance(..)
      , Mention(..)
      , Notification(..)
      , Relationship(..)
      , Report(..)
      , Results(..)
      , Status(..)
      , Tag(..)
      -- * Helpers
      , Acct(..)
      , AttachmentType(..)
      , NotificationType(..)
      , Visibility(..)
    ) where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T (Text)
import Lens.Micro
import Lens.Micro.TH
import qualified Time.Types as Time


data Account = Account
  {
    _accountId :: Int
  , _accountUsername :: Text
  , _accountAcct :: Acct
  , _accountDisplayName :: Text
  , _accountNote :: Text
  , _accountUrl :: Url
  , _accountAvatar :: Url
  , _accountHeader :: Url
  , _accountLocked :: Boolean
  , _accountCreatedAt :: Time.DateTime
  , _accountFollowersCount :: Int
  , _accountFollowingCount :: Int
  , _accountStatusesCount :: Int
  } deriving (Eq, Show)

instance FromJSON Account where
  parseJSON (Object o) = checkError o >>
    Account <$> o .: "id"
            <*> o .: "username"
            <*> (o .: "acct" >>= toAcct)
            <*> o .: "display_name"
            <*> o .: "note"
            <*> (o .: "url" >>= toUrl)
            <*> (o .: "avatar" >>= toUrl)
            <*> (o .: "header" >>= toUrl)
            <*> o .: "locked"
            <*> (o .: "created_at" >>= toCreatedAt)
            <*> o .: "followers_count"
            <*> o .: "following_count"
            <*> o .: "statuses_count"
  parseJSON v = fail $ "couldn't parse status from " ++ show v

makeLenses ''Account

data Acct = Acct
  {
    _acctUsername :: Text
  , _acctDomain :: Maybe Text
  } deriving (Eq, Show)

data Application = Application
  {
    _applicationName :: Text
  , _applicationWebsite :: Url
  } deriving (Eq, Show)

data Attachment = Attachment
  {
    _id :: Int
  , _type :: AttachmentType
  , _url :: Url
  , remoteUrl :: Url
  , previewUrl :: Url
  , textUrl :: Url
  } deriving (Eq, Show)

data AttachmentType = Image | Video | Gifv
  deriving (Eq, Show)

data Card = Card
  {
    _url :: Url
  , _title :: Text
  , _description :: Text
  , _image :: Url
  } deriving (Eq, Show)

data Context = Context
  {
    _ancestors :: List Status
  , _descendants :: List Status
  } deriving (Eq, Show)

data Error = Error { _error :: Text }
  deriving (Eq, Show)

data Instance = Instance
  {
    _uri :: Uri
  , _title :: Text
  , _description :: Text
  , _email :: Text
  } deriving (Eq, Show)

data Mention = Mention
  {
    _url :: Url
    , _username :: Text
    , _acct :: Acct
    , _id :: Int
  } deriving (Eq, Show)

data Notification = Notification
  {
    _id :: Int
  , _type :: NotificationType
  , createdAt :: Time.DateTime
  , account :: Account
  , status :: Status
  } deriving (Eq, Show)

data NotificationType = Mention | Reblog | Favourite | Follow
  deriving (Eq, Show)

data Relationship = Relationship
  {
    _following :: Boolean
  , _followedBy :: Boolean
  , _blocking :: Boolean
  , _muting :: Boolean
  , requested :: Boolean
  } deriving (Eq, Show)

data Report = Report
  {
    _id :: Int
  , _actionTaken :: Text
  } deriving (Eq, Show)

data Results = Results
  {
    _accounts :: List Account
  , _statuses :: List Status
  , _hashtags :: List Text
  } deriving (Eq, Show)

data Status = Status
  {
    _id :: Int
  , _uri :: Uri
  , _url :: Url
  , _account :: Account
  , _inReplyToId :: Maybe Int
  , _inReplyToAccountId :: Maybe Int
  , _reblog :: Maybe Status
  , _content :: Text
  , createdAt :: Time.DateTime
  , _reblogsCount :: Int
  , _favouritesCount :: Int
  , _reblogged :: Boolean
  , _favourited :: Boolean
  , _sensitive :: Boolean
  , _spoilerText :: SpoilerText
  , _visibility :: Visibility
  , _mediaAttachments :: List Attachment
  , _mentions :: List Mention
  , _tags :: List Tag
  , _application :: Application
  } deriving (Eq, Show)

data Visibility = Public | Unlisted | Private | Direct
  deriving (Eq, Show)

data Tag = Tag
  {
    _name :: Text
  , _url :: Url
  } deriving (Eq, Show)


type SpoilerText = Maybe Text
type Url = Text
type Uri = Text


-- stolen from https://hackage.haskell.org/package/twitter-types-0.7.2.2/docs/src/Web-Twitter-Types.html
checkError :: Object -> Parser ()
checkError o = do
    err <- o .:? "error"
    case err of
       Just msg -> fail msg
       Nothing -> return ()

toAcct :: Text -> Parser Acct
toAcct = return . makeAcct . T.splitOn "@"

toUrl :: Text -> Parser Url
toUrl = return

toCreatedAt :: Text -> Parser Time.DateTime
toCreatedAt = undefined

