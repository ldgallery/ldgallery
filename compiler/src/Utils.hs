-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019  Pacien TRAN-GIRARD
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


module Utils
  ( conj, neg
  , unique
  , passthrough
  ) where


import qualified Data.List
import qualified Data.Set


-- predicates

conj :: (a -> Bool) -> (a -> Bool) -> a -> Bool
conj p q x = (p x) && (q x)

neg :: (a -> Bool) -> a -> Bool
neg p x = not (p x)


-- lists

unique :: Ord a => [a] -> [a]
unique = Data.Set.toList . Data.Set.fromList


-- monads

passthrough :: Monad m => (a -> m b) -> a -> m a
passthrough f a = return a >>= f >>= \_ -> return a
