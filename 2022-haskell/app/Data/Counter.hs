-- Taken from https://github.com/wei2912/counter

-- Original License:

-- The MIT License (MIT)
--
-- Copyright (c) 2017 Ng Wei En
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

module Data.Counter where

import qualified Data.Map.Strict as M
import qualified Data.List as L

{-|
  The Counter type is an alias
  of Data.Map.Strict.Map.
-}
type Counter k v = M.Map k v

{-|
  Initialize a counter with no keys.
-}
empty :: Counter k v
empty = M.empty

{-|
  Initialize a counter with a single
  instance of a key.
-}
singleton ::
    Num v
    => k -- ^ Key to be inserted.
    -> Counter k v -- ^ New counter.
singleton k = M.singleton k 1

{-|
  Increment the frequency count of a key
  with a specified value.
-}
updateWith ::
    (Ord k, Num v)
    => k -- ^ Key to be inserted.
    -> v -- ^ Specified frequency count.
    -> Counter k v -- ^ Old counter.
    -> Counter k v -- ^ New counter.
updateWith = M.insertWith (+)

{-|
  Increment the frequency count of a key by 1.
-}
update ::
    (Ord k, Num v)
    => k -- ^ Key to be inserted.
    -> Counter k v -- ^ Old counter.
    -> Counter k v -- ^ New counter.
update k = updateWith k 1

{-|
  Convert a list of items into a counter.
-}
count ::
    (Ord k, Num v)
    => [k] -- ^ List of keys.
    -> Counter k v -- ^ New counter.
count = L.foldl' (flip update) M.empty

{-|
  Returns the union of two counters.
-}
union ::
    (Ord k, Num v)
    => Counter k v -- ^ First counter.
    -> Counter k v -- ^ Second counter.
    -> Counter k v -- ^ Union of both counters.
union = M.unionWith (+)
