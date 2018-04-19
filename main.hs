import Cliente
import Produto

main = do
	print ("Digite a acao desejada.")
	print ("1- Adicionar registro")
	print ("2- Alterar registro")
	print ("3- Excluir registro")
	escolhaString <- getLine
	let escolha = (read escolhaString::Int)
	if (escolha == 1) then do
		print ("Escolha o registo a que deseja adicionar")
		print ("1- Cliente")
		print ("2- Produto")
		adicionarString <- getLine
		let adicionar = (read adicionarString::Int)
		if (adicionar == 1) then
			adicionarCliente
		else
			adicionarProduto
	else
		if (escolha == 2) then do
			print ("Escolha o registro que deseja alterar")
			print ("1- Cliente")
			print ("2- Produto")
			alterarString <- getLine
			let alterar = (read alterarString::Int)
			if (alterar == 1) then
				alterarCliente
			else
				alterarProduto
		else do
			print ("Escolha o registro que deseja remover")
			print ("1- Cliente")
			print ("2- Produto")
			removerString <- getLine
			let remover = (read removerString::Int)
			if (remover == 1) then
				removerCliente
			else
				removerProduto
