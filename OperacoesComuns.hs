module OperacoesComuns (novaId, pegarAtributo, quebrarString,
						buscarRegistro, alterarRegistro, removerRegistro)
						where

novaId :: [String] -> String
novaId (a:[]) = show ((read (pegarAtributo (quebrarString ',' a) 0)::Int)+1)::String
novaId (a:b) = novaId b

pegarAtributo :: [String] -> Int -> String
pegarAtributo (a:_) 0 = a
pegarAtributo (a:b) posicao = pegarAtributo b (posicao - 1)

quebrarString :: Char -> String -> [String]
quebrarString _ "" = []
quebrarString c s = let (l, s') = break (== c) s
						in l : case s' of
							[] -> []
							(_:s'') -> quebrarString c s''

buscarRegistro :: [String] -> Int -> String -> [String]
buscarRegistro [] _ _ = []
buscarRegistro (a:b) posicao comparador | (pegarAtributo (quebrarString ',' a) posicao == comparador) = quebrarString ',' a
										| otherwise = buscarRegistro b posicao comparador

alterarRegistro :: [String] -> String -> String -> [String]
alterarRegistro [] _ _ = []
alterarRegistro (a:b) novoRegistro id | (pegarAtributo (quebrarString ',' a) 0) == id = novoRegistro:(alterarRegistro b novoRegistro id)
									  | otherwise = a:(alterarRegistro b novoRegistro id)

removerRegistro :: [String] -> String -> [String]
removerRegistro [] _ = []
removerRegistro (a:b) id | (pegarAtributo (quebrarString ',' a) 0) == id = removerRegistro b id
						 | otherwise = a:(removerRegistro b id)