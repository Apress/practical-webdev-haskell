module Adapter.RabbitMQ.Common where
  
import ClassyPrelude
import Network.AMQP
import Data.Has
import Katip
import Data.Aeson

data State = State
  { statePublisherChan :: Channel
  , stateConsumerChan :: Channel
  }

type Rabbit r m =
  (Has State r, MonadReader r m, MonadIO m)

withState :: String -> Integer -> (State -> IO a) -> IO a
withState connUri prefetchCount action = bracket initState destroyState action'
  where
    initState = do
      publisher <- openConnAndChan
      consumer <- openConnAndChan
      return (publisher, consumer)

    openConnAndChan = do
      conn <- openConnection'' . fromURI $ connUri
      chan <- openChannel conn
      confirmSelect chan False
      qos chan 0 (fromInteger prefetchCount) True
      return (conn, chan)
      
    destroyState ((conn1, _), (conn2, _)) = do
      closeConnection conn1
      closeConnection conn2

    action' ((_, pubChan), (_, conChan)) = action (State pubChan conChan)

initQueue :: State -> Text -> Text -> Text -> IO ()
initQueue state@(State pubChan _) queueName exchangeName routingKey = do
  initExchange state exchangeName
  void $ declareQueue pubChan (newQueue { queueName = queueName })
  bindQueue pubChan queueName exchangeName routingKey

initExchange :: State -> Text -> IO ()
initExchange (State pubChan _) exchangeName = do
  let exchange = newExchange  { exchangeName = exchangeName
                              , exchangeType = "topic" }
  declareExchange pubChan exchange

initConsumer :: State -> Text -> (Message -> IO Bool) -> IO ()
initConsumer (State _ conChan) queueName handler =
  void . consumeMsgs conChan queueName Ack $ \(msg, env) -> void . fork $ do
    result <- handler msg
    if result then ackEnv env else rejectEnv env False

--- Higher level abstraction for publishing and consuming

publish :: (ToJSON a, Rabbit r m) => Text -> Text -> a -> m ()
publish exchange routingKey payload = do
  (State chan _) <- asks getter
  let msg = newMsg { msgBody = encode payload }
  liftIO . void $ publishMsg chan exchange routingKey msg

consumeAndProcess :: (KatipContext m, FromJSON a, MonadCatch m)
                  => Message -> (a -> m Bool) -> m Bool
consumeAndProcess msg handler =
  case eitherDecode' (msgBody msg) of
    Left err -> withMsgAndErr msg err $ do
      $(logTM) ErrorS "Malformed payload. Rejecting."
      return False
    Right payload -> do
      result <- tryAny (handler payload)
      case result of
        Left err -> withMsgAndErr msg (displayException err) $ do
          $(logTM) ErrorS "There was an exception when processing the msg. Rejecting."
          return False
        Right bool ->
          return bool

withMsgAndErr :: (KatipContext m, ToJSON e) => Message -> e -> m a -> m a
withMsgAndErr msg err =
  katipAddContext (sl "mqMsg" (show msg) <> sl "error" err)