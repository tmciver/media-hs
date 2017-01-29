module EventSourcing.Entity where

type EntityId = String

data Entity entity command event = Entity { _entityId :: entity -> EntityId
                                          , _init :: entity
                                          , _apply :: entity -> event -> Either String entity
                                          , _handle :: command -> IO (Either String [event])
                                          }

entityId :: Entity entity command event -> entity -> EntityId
entityId entityI entity = _entityId entityI entity

init :: Entity entity command event -> entity
init entityI = _init entityI

apply :: Entity entity command event -> entity -> event -> Either String entity
apply entityI entity event = _apply entityI entity event

handle :: Entity entity command event -> command -> IO (Either String [event])
handle entityI command = _handle entityI command
