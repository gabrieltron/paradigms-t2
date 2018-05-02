module OperacoesComuns (novaId, pegarAtributo, quebrarString,
						buscarRegistro, alterarRegistro, removerRegistro,
						arredondarFloat, date, buscarNRegistros,
						imprimir, criarData)
						where

import Text.Printf
import Data.Time.Clock
import Data.Time.Calendar

criarData :: String -> Day
criarData dataString = do
	let dataInicial = quebrarString '/' dataString
	let dia1 = (read (dataInicial!!0)::Int)
	let mes1 = (read (dataInicial!!1)::Int)
	let ano1 = (read (dataInicial!!2)::Integer)
	fromGregorian ano1 mes1 dia1

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

arredondarFloat :: Float -> Float
arredondarFloat float = read (printf "%.2f" (float :: Float))::Float

novaId :: [String] -> String
novaId [] = "1"
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

buscarNRegistros :: [String] -> Int -> String -> [String]
buscarNRegistros [] _ _ = []
buscarNRegistros (a:b) posicao comparador | (pegarAtributo (quebrarString ',' a) posicao == comparador) = a:(buscarNRegistros b posicao comparador)
										| otherwise = buscarNRegistros b posicao comparador

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

imprimir :: [String] -> IO ()
imprimir [] = return ()
imprimir (a:b) = do
	print (a)
	imprimir (b)