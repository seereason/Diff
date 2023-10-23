# 0.5

  - Bring space complexity down to D^2 (Bodigrim).
  - Add `Bifunctor` instance (Jonathan King).  Requires `base` >= 4.8.
  - Fix for the grouped context diff.  It was omitting all trailing contexts.
  - Allow unlimited number of context elements (`getContextDiffNew`).

# 0.4

  - Generalize `Diff a` to `PolyDiff a b`.
    `Diff` has been replaced with a specialized synonym `type Diff a = PolyDiff a a`,
    but it's still not backward compatible if you imported `Diff(..)`.
