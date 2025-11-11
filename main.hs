{-# LANGUAGE DeriveGeneric #-}
-- Extensão de linguagem que permite derivar automaticamente a classe 'Generic'.
-- Isso é útil quando queremos compatibilidade com bibliotecas de serialização,

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import Control.Exception (catch, IOException)
import Data.Time.Clock (getCurrentTime)


-- -------------------------Tipos principais do sistema de inventário ----------------------

-- -------Representa um item individual no inventário

data Item = Item {
    itemID    :: String,   
    nome      :: String,   
    quantidade :: Int,     
    categoria :: String    
}deriving (Show, Read, Eq, Generic) -- aqui aplico as implementações necessárias:
                                    --   Show: permite converter o item em texto 
                                    --   Read: permite ler um item a partir de texto com 'read'
                                    --   Eq: permite comparar dois itens 
                                    --   Generic: habilita serialização automática




-- --------Tipo Inventario:

-- O inventário é um Map que associa a chaves a valores, ou seja 
--Inventario = dicionário de itens
type Inventario = Map String Item



-- --------Tipo AcaoLog

-- Define um tipo enumerado com todas as ações possíveis que 
-- podem ocorrer no sistema de inventário
data AcaoLog
    = Add           -- Quando um item é adicionado ao inventário
    | Remove        -- Quando um item é removido do inventário
    | Update        -- Quando a quantidade de um item é alterada
    | QueryFail     -- Quando uma consulta ou operação falha
    deriving (Show, Read, Eq, Generic) -- As mesmas implemetações são aplicadas aqui:



-- --------Tipo StatusLog

-- É o resultado de uma operação registrada no log, podendo
-- ser sucesso ou falha com uma mensagem explicando o erro
data StatusLog
    = Sucesso          
    | Falha String     
    deriving (Show, Read, Eq, Generic) -- As mesmas implemetações são aplicadas aqui:


-- --------Tipo LogEntry

-- Registra as entradas de log, possuindo informações
-- sobre uma operação realizada no sistema
data LogEntry = LogEntry {
    timestamp :: UTCTime,  -- Data e hora em que a operação ocorreu
    acao      :: AcaoLog,  -- Tipo de ação (Add, Remove, Update)
    detalhes  :: String,   -- Descrição textual da operação
    status    :: StatusLog -- Resultado da operação (Sucesso ou Falha)
}deriving (Show, Read, Eq, Generic) -- As mesmas implemetações são aplicadas aqui:



-- ----------------------------- Funções Puras de Transação -----------------------------

-- ----------Tipo ResultadoOperacao: tipo auxiliar

-- é o resultado de uma função de transação: novo inventário + log.
type ResultadoOperacao = (Inventario, LogEntry)



-- ----------Função addItem
-- Adiciona um novo item ao inventário. Recebe como argumentos: UTCTime (o horário da operação);
-- Item (o item a ser adicionado); Inventario (o estado atual do inventário)
-- e retorna o resultado da operação (Either String ResultadoOperacao)

-- Valida se o ID já existe. Caso exista, retorna erro (Left).
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao -- essa é a assinatura da função 
addItem time item inv =
    if Map.member (itemID item) inv -- Verifica se o itemID já existe no Map (inv é um Map String Item). Retorna True se o item já está cadastrado (executando o then) e False caso contrário (executando o else).
        then 
            let logFail = LogEntry time Add -- Cria uma entrada de log (logFail) com: time: momento atual; Add: tipo de ação; Uma mensagem de falha ("ID já existe") e um StatusLog de falha.
                        ("Falha ao adicionar: ID " ++ itemID item ++ " já existe.")
                        (Falha "ID duplicado")
                in Left "ID duplicado" -- Retorna Left "ID duplicado" (ou seja, falha)


        else -- Se o ID não existe, Usa Map.insert para inserir o novo item no inventário
            let novoInv = Map.insert (itemID item) item inv
                logOk = LogEntry time Add -- Cria um log de sucesso (logOk)
                    ("Item " ++ nome item ++ " adicionado com sucesso.")
                    Sucesso
            in Right (novoInv, logOk) -- Retorna esse resultadode operação
            
            
            
            
-- --------- Função removeItem

-- Remove um item existente do inventário e retorna erro caso o ID não exista
-- Recebe como argumentos o UTCTime; o ID do item e o estado atual do inventario

removeItem :: UTCTime -> String -> Inventario -> Either String ResultadoOperacao
removeItem time idItem inv =
    case Map.lookup idItem inv of -- faz uma conferência de existência do ID no inventário diferente da anterior, permitindo acessar seu valor caso encontrada
        Nothing ->
            -- Caso não encontre: cria um log de falha, retornndo Left "Item inexistente"
            let logFail = LogEntry time Remove
                    ("Falha ao remover: ID " ++ idItem ++ " não encontrado.")
                    (Falha "Item inexistente")
            in Left "Item inexistente"
        Just item ->
            -- Caso encontre: remove o item com Map.delete, cria um log de sucesso e retorna Right (novoInv, logOk).
            let novoInv = Map.delete idItem inv
                logOk = LogEntry time Remove
                    ("Item " ++ nome item ++ " removido com sucesso.")
                    Sucesso
            in Right (novoInv, logOk)
            
            
            
            

-- -------- Função updateQty

-- Atualiza a quantidade de um item no inventário.
-- Retorna erro se o item não existir ou se a nova quantidade for negativa.

-- Recebe como argumentos o UTCTime; o ID do item, a nova quantidade e o estado atual do inventario

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty time idItem novaQtd inv =
    case Map.lookup idItem inv of -- Busca o item 
        Nothing -> -- se não existir, retorna Left, gerando um log de falha e uma mensagem de erro
            let logFail = LogEntry time Update
                    ("Falha ao atualizar: ID " ++ idItem ++ " não encontrado.")
                    (Falha "Item inexistente")
            in Left "Item inexistente"

        Just item -> -- se existir, 
            if novaQtd < 0  -- Verifica se novaQtd é negativa 
                then -- Se for, retorna falha, Left, com mensagem de "Quantidade negativa").
                    let logFail = LogEntry time Update
                            ("Falha ao atualizar " ++ nome item ++ ": quantidade inválida.")
                            (Falha "Quantidade negativa")
                    in Left "Quantidade negativa"
                else -- Se não for, atualiza o registro do item e o inventário também
                    let itemAtualizado = item { quantidade = novaQtd }
                        novoInv = Map.insert idItem itemAtualizado inv -- insere o item atualizado no inventário
                        logOk = LogEntry time Update
                            ("Quantidade de " ++ nome item ++ " atualizada para " ++ show novaQtd ++ ".")
                            Sucesso -- cria um log de sucesso
                    in Right (novoInv, logOk) -- Retorna Right (novoInv, logOk).
                    
                    
                    
                    
-- --------------------------------Main e Loop de Estado------------------------------

-- ---------Leitura e Escrita de Arquivos:

-- utilizando aliases, nomeamos os caminhos dos arquivos usados pelo programa:

-- caminho do arquivo que guarda o inventário serializado
inventarioFile :: FilePath
inventarioFile = "Inventario.dat" -- caminho relativos ao diretório atual do processo

-- caminho do arquivo que guarda o log/auditoria, sendo uma linha por LogEntry
logFile :: FilePath
logFile = "Auditoria.log" -- caminho relativos ao diretório atual do processo

-- aqui é feita a leitura do inventário do arquivo, tratando erro com 'catch'
carregarInventario :: IO Inventario
carregarInventario = do
    -- tenta ler o conteúdo de texto do arquivo inventarioFile
    conteudo <- catch (readFile inventarioFile) handler
    -- converte o texto lido para o valor Haskell usando 'read' -- tenta interpretar a string como um valor do tipo Inventario
    return (read conteudo)
  where
    --se ocorrer IOException se arquivo não existir, handler é executado 
    handler :: IOException -> IO String
    handler _ = return "Map.empty" -- aqui é retornado a string "Map.empty" para que 'read' produza Map.empty



-- Não carrega o log na memória, apenas garante que exista.
-- Usa catch para não falhar se não existir
carregarLog :: IO ()
carregarLog = catch (readFile logFile >> return ()) handler -- readFile logFile lê o arquivo e >> return () descarta o conteúdo e devolve ()
  where
    handler :: IOException -> IO ()
    -- se ocorrer exceção (caso o arquivo não exista), handler cria um arquivo vazio com writeFile ""
    handler _ = writeFile logFile ""




-- Serializa o inventario com show e escreve no arquivo, 
-- sobrescrevendo o conteúdo que tinha antes com o show inv
salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile inventarioFile (show inv)


-- Serializa LogEntry com show e acrescenta no final de Auditoria.log, mantendo uma entrada por linha.
salvarLog :: LogEntry -> IO ()
salvarLog logEntry = appendFile logFile (show logEntry ++ "\n")
-- appendFile adiciona ao final do arquivo; cada LogEntry fica em uma linha




-- ------------------ Parser e Execução de Comandos

-- Formato esperado de comandos via terminal:
-- add <id> <nome> <qtd> <cat>
-- remove <id>
-- update <id> <qtd>
-- exit

-- processarComando recebe:
--   UTCTime  -> momento atual (para o log)
--   [String] -> comando tokenizado (ex.: words linha)
--   Inventario -> estado atual
-- retorna: IO Inventario (novo estado, possivelmente igual ao anterior)



-- Essa função é o que une a camada pura e a camada IO
processarComando :: UTCTime -> [String] -> Inventario -> IO Inventario

-- Caso o usuário não insira comandos, é impresso "Comando vazio." no terminal
-- e retorna o inventário sem alteracoes
processarComando _ [] inv = do
    putStrLn "Comando vazio."
    return inv


-- -------------------- ADD --------------------
-- Pattern matching: "add":idItem:nome:qtdStr:cat:_  => pega os primeiros 4 tokens
-- corresponde apenas se houver pelo menos 4 tokens após "add".

-- idItem: 1º token depois do "add"
-- nome : 2º token (aqui os nomes tem que estar entre áspas).
-- qtdStr :3º token (string a ser parseada como Int).
-- cat: 4º token (categoria).
-- :_ ignora quaisquer tokens adicionais.

processarComando time ("add":idItem:nome:qtdStr:cat:_) inv =
    case reads qtdStr :: [(Int, String)] of    -- reads tenta parsear qtdStr como Int e retorna lista de pares (valor, String), 
        [(qtd, "")] -> do --   o formato [(valor,"")] indica sucesso
            let item = Item idItem nome qtd cat  -- cria Item a partir dos tokens
            case addItem time item inv of  -- chama função pura addItem
            
                -- caso de erro:
                Left msg -> do
                    let logFail = LogEntry time Add msg (Falha msg) -- escreve entrada de falha no log
                    salvarLog logFail -- grava log de falha
                    putStrLn ("Erro: " ++ msg) -- printa mensagem de falha
                    return inv      -- retora o inventário sem alteracoes
                    
                -- caso de certo:                         
                Right (novoInv, logOk) -> do
                    salvarInventario novoInv        -- sobrescreve inventário 
                    salvarLog logOk                 -- escreve entrada de sucesso no log
                    putStrLn "Item adicionado com sucesso!" -- printa aviso de sucesso
                    return novoInv                  -- retorna o novo inventário
                    
                    
        _ -> do
            let msg = "Quantidade inválida."
            let logFail = LogEntry time Add msg (Falha msg)  -- escreve entrada de falha no log
            salvarLog logFail       -- adiciona a falha do log
            putStrLn msg   -- printa mensagem de falha
            return inv -- retorna o inventario sem alteracoes
            
            
            
            

-- -------------------- REMOVE --------------------
-- Pattern matching exige o comando "remove" seguido de pelo menos um token (o idItem).
processarComando time ("remove":idItem:_) inv =
    case removeItem time idItem inv of                     -- chama função pura removeItem
    
    -- Caso de erro:
        Left msg -> do
            let logFail = LogEntry time Remove msg (Falha msg)      -- escreve entrada de falha no log
            salvarLog logFail                                       -- grava log de falha
            putStrLn ("Erro: " ++ msg)                      -- printa mensagem de falha
            return inv                                     -- retorna inventario inalterado
            
    -- Caso de Sucesso:
        Right (novoInv, logOk) -> do
            salvarInventario novoInv                       --  salva inventário atualizado
            salvarLog logOk                                 -- grava log de sucesso
            putStrLn "Item removido com sucesso!"          -- printa mensagem de sucesso
            return novoInv                                  -- retorna inventario novo 






-- -------------------- UPDATE --------------------
-- Pattern matching: precisa de pelo menos 2 tokens depois de "update": idItem e novaQtdStr
processarComando time ("update":idItem:novaQtdStr:_) inv =

    case reads novaQtdStr :: [(Int, String)] of           -- Validação da novaQtdStr com o reads usando a mesma lógica do add
        [(novaQtd, "")] -> do --   o formato [(valor,"")] indica sucesso
            case updateQty time idItem novaQtd inv of      -- chama função pura updateQty
            
            -- caso de erro:
                Left msg -> do
                    let logFail = LogEntry time Update msg (Falha msg) -- escreve entrada de falha no log
                    salvarLog logFail                               -- grava log de falha
                    putStrLn ("Erro: " ++ msg)                  -- printa mensagem de falha
                    return inv                                 -- retorna inventário sem alteracoes
                    
            -- caso de sucesso:
                Right (novoInv, logOk) -> do
                    salvarInventario novoInv               -- salva inventário novo
                    salvarLog logOk                         -- grava log de sucesso
                    putStrLn "Quantidade atualizada!"     -- printa mensagem de sucesso
                    return novoInv                      -- retorna inventário alterado 
                    
                    
        -- caso não seja um Int válido o parse da quantidade falha ""           
        _ -> do
            let msg = "Quantidade inválida."
            let logFail = LogEntry time Update msg (Falha msg) -- escreve entrada de falha no log
            salvarLog logFail                               -- grava log de falha
            putStrLn msg                        -- printa mensagem de falha
            return inv                          -- retorna inventário inalterado





-- -------------------- EXIT --------------------
processarComando _ ["exit"] inv = do        -- Se o tokenizado for exatamente ["exit"]:  
    putStrLn "Encerrando programa..."    -- printa mensagem de encerramento
    return inv                          -- devolve inventário atual sem alterá-lo





-- -------------------- ETC --------------------

-- Qualquer outra entrada, seja número errado de tokens, comando desconhecido, etc, cai aqui 

processarComando _ _ inv = do
    let msg = "Comando inválido."
    time <- getCurrentTime              -- obtém o horário atual do sistema e atribui à variável time
    let logFail = LogEntry time QueryFail msg (Falha msg) -- escreve entrada de falha no log
    salvarLog logFail       -- grava log de falha
    putStrLn msg        -- printa mensagem de falha
    return inv             -- devolve inventário atual sem alterá-lo
    
    
    
    



-- --------------------------------Loop Principal ------------------------------

-- Recebe o estado atual do inventario e devolve uma ação IO () que faz o loop ser interativo
loopPrincipal :: Inventario -> IO ()
loopPrincipal inv = do  -- Abre um do-block para sequenciar ações IO, inv sendo o inventário atual
    putStr "\n-  "           -- linha que mostra para o usuário onde digitar
    hFlush stdout           -- garante que a linha para o usuário apareça direto sem buffer
    linha <- getLine        -- lê a linha digitada pelo usuário. O resultado "linha" é a string digitada
    if linha == "exit"      -- checa se o usuário digitou a opçao "exit" que fecha o programa
        then putStrLn "Saindo..." -- printa mensagem de saida
        
        -- caso o usuário não tenha digitado o comando de saida:
        else do
            time <- getCurrentTime                         -- obtém UTCTime atual que será usado no log
            novoInv <- processarComando time (words linha) inv -- tokeniza (separa pelos espaços) e processa o comando digitado
            loopPrincipal novoInv   -- O loop é recursivo, recebendo um inventário e, a cada iteração, se chama novamente com o novo inventário 





-- ---------- Ponto de entrada Main ------------

-- Ponto de entrada do programa é uma ação IO executada quando o programa inicia
main :: IO ()
main = do

-- Impressão de cabeçalho e instruções para o usuário do que o programa espera no terminal (muito importante para UX em CLI/GDB Online).
    putStrLn "======================================="
    putStrLn "   Sistema de Inventário - Haskell  "
    putStrLn "======================================="
    putStrLn " Atenção!! os nomes dos produtos NÃO podem conter espaço!"
    
    putStrLn "\nComandos disponíveis:"
    putStrLn " add <id> <nome> <qtd> <cat>"
    putStrLn " remove <id>"
    putStrLn " update <id> <qtd>"
    putStrLn " exit"
    putStrLn "----------------------------------------"

    -- Inicialização: garante que o arquivo de log exista e carrega o inventário
    carregarLog                   -- cria Auditoria.log vazio caso não exista ainda
    inventario <- carregarInventario -- tenta ler Inventario.dat. Se não tiver, usa Map.empty

    -- Inicia o loop de interação com o estado inicial carregado
    loopPrincipal inventario    
    

            
