data Campaign = Campaign String Double

class JsonViewable a where
    toJson :: a -> String

instance JsonViewable Campaign where
    toJson (Campaign name budget) = concat ("{\"name\":\"}" : name : "\",\"budget\":" : (show budget) : "}" : []);

produceJson :: (JsonViewable a) => a -> String
produceJson a = toJson a

main = print(produceJson(Campaign "Test campaign" 123))
