module EventSourcing.Entity where

-- class Entity id event where
--   entityId :: id

--   apply :: event -> Entity id

type EntityId = String
-- data Entity entity command event = Entity { entityId :: forall a . a
--                                  , apply :: event -> Entity entity event
--                                  }

data Entity entity event = Entity { _entityId :: entity -> EntityId
                                  , _apply :: entity -> event -> Either String entity
                                  }

getEntityId :: Entity entity event -> entity -> EntityId
getEntityId entityI entity = _entityId entityI entity

applyEvent :: Entity entity event -> entity -> event -> Either String entity
applyEvent entityI entity event = _apply entityI entity event
