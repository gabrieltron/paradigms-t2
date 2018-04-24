import OperacoesComuns
import Cliente
import Produto
import Venda
import System.Exit
import Control.DeepSeq
import Control.Exception

escolherAdicionar :: IO ()
escolherAdicionar = do
	print ("Escolha o registo a que deseja adicionar")
	print ("1- Cliente")
	print ("2- Produto")
	print ("3- Venda")
	adicionarString <- getLine
	let adicionar = (read adicionarString::Int)
	if (adicionar == 1) then
		adicionarCliente
	else
		if (adicionar == 2) then
			adicionarProduto
		else
			registrarVenda

escolherEditar :: IO ()
escolherEditar = do
	print ("Escolha o registro que deseja alterar")
	print ("1- Cliente")
	print ("2- Produto")
	alterarString <- getLine
	let alterar = (read alterarString::Int)
	if (alterar == 1) then
		alterarCliente
	else
		alterarProduto

escolherRemover :: IO ()
escolherRemover = do
	print ("Escolha o registro que deseja remover")
	print ("1- Cliente")
	print ("2- Produto")
	removerString <- getLine
	let remover = (read removerString::Int)
	if (remover == 1) then
		removerCliente
	else
		removerProduto

main = do
	-- cria db caso nao existam
	appendFile "cliente.db" ""
	appendFile "produto.db" ""
	appendFile "venda.db" ""
	appendFile "itemvenda.db" ""

	print ("Digite a acao desejada.")
	print ("1- Adicionar registro")
	print ("2- Alterar registro")
	print ("3- Excluir registro")
	escolhaString <- getLine
	let escolha = (read escolhaString::Int)
	if (escolha == 1) then do
		escolherAdicionar
	else
		if (escolha == 2) then do
			escolherEditar
		else do
			escolherRemover
