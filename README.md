# review-rgb-ranges
Provides an [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to detect `rgb`, `rgba`, `rgb255` and `rgba255` values from [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) out of range.

## Example configuration

```elm
module ReviewConfig exposing (config)

import NoInvalidRGBValues
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoInvalidRGBValues.rule
    ]
```

