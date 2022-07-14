# 01 Add Routes

Evolve that hello world to a site with two routes, each with different HTML.

For mood tracker, we want

```haskell
data Route
  = Route_Index 
  | Route_Day Day
  | Route_Cal
```