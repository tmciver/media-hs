module EventSourcing.Entity where

type EntityId = String

data Entity entity event = Entity { _entityId :: entity -> EntityId
                                  , _apply :: entity -> event -> Either String entity
                                  }

entityId :: Entity entity event -> entity -> EntityId
entityId entityI entity = _entityId entityI entity

apply :: Entity entity event -> entity -> event -> Either String entity
apply entityI entity event = _apply entityI entity event
