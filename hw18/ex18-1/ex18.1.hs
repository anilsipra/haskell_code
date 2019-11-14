import Text.ParserCombinators.ReadP

greedy :: ReadP a -> ReadP [a]
greedy parser = pure (:) <*> (parser) <*> (greedy parser <++ pure [])