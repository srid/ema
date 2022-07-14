# 02 Add a Model
Now add a `Model` type using `WithModel`, but a constant Dynamic.

```haskell
data Model = Model 
  { modelDays :: Map Day Mood 
  }

data Mood = Bad | Neutral | Good
```