-- Most of this code has been lifted from here: http://www.hyperlambda.com/posts/cqrs-es-in-haskell/

newtype AggregateId = AggregateId String deriving (Eq, Show, Ord, Generic)

aggregateIdFromString :: String -> Maybe AggregateId
aggregateIdFromString "" = Nothing
aggregateIdFromString s = Just $ AggregateId s

-- Question: how do we identify a media file? I heard somewhere that large files
-- like photos and videos should not be stored in the events themselves but
-- should be put into a document repository. In that case I suppose we'd have
-- some kind of ID or URL to the file that we could store in the events.
data Event = MediaWasAdded AggregateId
           | MediaWasDeleted AggregateId
           deriving (Eq, Show, Generic)

-- defined near the bottom of the page of the link above.
type RouteParameter = String
type RouteParameterValue = String
type RouteParameters = Map.Map RouteParameter RouteParameterValue

-- I don't like the name of the type "RouteParameters". It's too close to HTTP
-- which should not be mentioned in the domain model. And "RouteParameters" is
-- a Map of String to String; that doesn't seem very type safe and rather too
-- generic. But how else to do it?  Do we need to define CommandHanlder as a
-- type?  Why not just use regular functions for commands (with different type
-- signatures)?  Here:
-- http://www.jayway.com/2014/02/25/cqrs-in-haskell-command-validation-with-applicative-functors/
-- commands are defined in a way that I don't understand but it's different
-- than above.
type CommandHandler = [Event] -> RouteParameters -> Maybe [Event]

addMediaHandler :: CommandHandler
addMediaHandler events args = do
  aggregateId <- aggregateIdFromString =<< Map.lookup "id" args
  validateThatIsNew aggregateId events
  return [MediaWasAdded aggregateId]
    where
      validateThatIsNew aggregateId = guard . notElem (CounterCreated aggregateId)

-- similarly for deleteMediaHandler
