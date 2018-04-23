module Produto (adicionarProduto, alterarProduto, removerProduto,
				escreverAlteracaoProduto) where

import OperacoesComuns
import Control.DeepSeq
import Control.Exception

adicionarProduto :: IO ()
adicionarProduto = do
	arquivo <- readFile "produto.db"
	let produtos = lines arquivo
	let codigo = novaId produtos
	print ("Digite o nome")
	nome <- getLine
	print ("Digite a quantidade em estoque")
	quantidade <- getLine
	print ("Digite o preco unitario ou por Kilo")
	preco <- getLine

	evaluate (force arquivo)
	appendFile "produto.db" (""++codigo++",\""++nome++"\","++quantidade++","++preco++"\n")

alterarProduto :: IO ()
alterarProduto = do
	print ("Digite o id a ser alterado")
	id <- getLine
	print ("Digite o novo nome")
	nome <- getLine
	print ("Digite a nova quantidade em estoque")
	quantidade <- getLine
	print ("Digite o novo preco unitario ou por Kilo")
	preco <- getLine
	escreverAlteracaoProduto id nome quantidade preco

escreverAlteracaoProduto :: String -> String -> String -> String -> IO ()
escreverAlteracaoProduto id nome quantidade preco = do
	arquivo <- readFile "produto.db"
	let produtos = lines arquivo
	evaluate (force arquivo)
	let novoRegistro = (""++id++",\""++nome++"\","++quantidade++","++preco)
	let produtosAlterado = alterarRegistro produtos novoRegistro id
	writeFile "produto.db" (unlines produtosAlterado)


removerProduto :: IO ()
removerProduto = do
	arquivoProdutos <- readFile "produto.db"
	arquivoItemVendas <- readFile "itemvenda.db"
	let produtos = lines arquivoProdutos
	let itemVendas = lines arquivoItemVendas
	print ("Digite o id a ser excluido")
	id <- getLine
	if (produtoSemVendas itemVendas id) then do
		let produtosAlterado = removerRegistro produtos id
		evaluate (force arquivoProdutos)
		writeFile "produto.db" (unlines produtosAlterado)
	else
		print ("Esse produto possui vendas. Nao e possivel excluir")

produtoSemVendas :: [String] -> String -> Bool
produtoSemVendas [] _ = True
produtoSemVendas (a:b) id | (pegarAtributo (quebrarString ',' a) 2) == id = False
					  | otherwise = produtoSemVendas b id