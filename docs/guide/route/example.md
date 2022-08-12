---
order: 100
---

# Example route

```haskell
-- An example of nested routes
-- Route's expected encoding is given as a comment.

data Route
  = Route_Index          -- index.html
  | Route_About          -- about.html
  | Route_Contact        -- contact.html
  | Route_Blog BlogRoute -- blog/<BlogRoute>

data BlogRoute
  = BlogRoute_Index      -- index.html
  | BlogRoute_Post Slug  -- post/<Slug>

newtype Slug = Slug { unSlug :: String }
```