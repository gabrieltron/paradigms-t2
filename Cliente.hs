module Cliente (adicionarCliente, alterarCliente, removerCliente) where

import OperacoesComuns
import Control.DeepSeq
import Control.Exception

adicionarCliente :: IO ()
adicionarCliente = do
	clientesFile <- readFile "cliente.db"
	let clientes = lines clientesFile
	let codigo = novaId clientes
	print ("Digite o nome")
	nome <- getLine
	print ("Digite a cidade")
	cidade <- getLine
	print ("Digite a idade")
	idade <- getLine
	print ("Digite o sexo")
	sexo <- getLine

	evaluate (force clientesFile)
	appendFile "cliente.db" (""++codigo++","++nome++","++cidade++","++idade++","++sexo++"\n")

alterarCliente :: IO ()
alterarCliente = do
	arquivo <- readFile "cliente.db"
	let clientes = lines arquivo
	print ("Digite o id a ser alterado")
	id <- getLine
	print ("Digite um novo nome")
	nome <- getLine
	print ("Digite um novo cidade")
	cidade <- getLine
	print ("Digite um novo idade")
	idade <- getLine
	print ("Digite um novo sexo")
	sexo <- getLine
	let novoRegistro = (""++id++","++nome++","++cidade++","++idade++","++sexo)
	let clientesAlterado = alterarRegistro clientes novoRegistro id 
	evaluate (force arquivo)
	writeFile "cliente.db" (unlines clientesAlterado)

removerCliente :: IO ()
removerCliente = do
	arquivoClientes <- readFile "cliente.db"
	arquivoVendas <- readFile "venda.db"
	let clientes = lines arquivoClientes
	let vendas = lines arquivoVendas
	print ("Digite o id a ser excluido")
	id <- getLine
	if (clienteSemVendas vendas id) then do
		let clientesAlterado = removerRegistro clientes id
		evaluate (force arquivoClientes)
		writeFile "cliente.db" (unlines clientesAlterado)
	else
		print ("Esse cliente possui vendas. Nao e possivel excluir")

clienteSemVendas :: [String] -> String -> Bool
clienteSemVendas [] _ = True
clienteSemVendas (a:b) id | (pegarAtributo (quebrarString ',' a) 1) == id = False
					  | otherwise = clienteSemVendas b id
