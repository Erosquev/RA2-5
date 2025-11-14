{-# LANGUAGE DeriveGeneric #-}
-- Extensão de linguagem que permite derivar automaticamente a classe 'Generic'.
-- Isso é útil quando queremos compatibilidade com bibliotecas de serialização.

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import Control.Exception (catch, IOException, evaluate)
import Data.Time.Clock (getCurrentTime)
import Data.List (maximumBy)
import Data.Ord (comparing)



-- -------------------------Tipos principais do sistema de inventário ----------------------

-- -------Representa um item individual no inventário

data Item = Item {
    itemID    :: String,    -- Identificador único do item
    nome      :: String,    -- Nome do item
    quantidade :: Int,      -- Quantidade atual em estoque
    categoria :: String     -- Categoria a que pertence
}deriving (Show, Read, Eq, Generic) -- Implementações derivadas necessárias:
                                    --   Show: permite converter o item em texto (para salvar em arquivo)
                                    --   Read: permite ler um item a partir de texto (ao carregar o arquivo)
                                    --   Eq: permite comparar dois itens
                                    --   Generic: habilita serialização automática (usada em bibliotecas externas, se necessário)




-- --------Tipo Inventario:

-- O inventário é um Map (estrutura de dicionário) que associa chaves a valores.
-- A chave é o itemID (String), e o valor é o próprio Item.
type Inventario = Map String Item




-- --------Tipo AcaoLog

-- Define um tipo enumerado com todas as ações possíveis que 
-- podem ocorrer no sistema de inventário.
data AcaoLog
    = Add           -- Quando um item é adicionado ao inventário
    | Remove        -- Quando um item é removido do inventário
    | Update        -- Quando a quantidade de um item é alterada
    | QueryFail     -- Quando uma consulta ou operação falha
    deriving (Show, Read, Eq, Generic) -- As mesmas implementações derivadas se aplicam aqui:
                                       -- Show, Read, Eq e Generic




-- --------Tipo StatusLog

-- É o resultado de uma operação registrada no log, podendo
-- ser sucesso ou falha com uma mensagem explicando o erro.
data StatusLog
    = Sucesso           -- Representa uma operação bem-sucedida
    | Falha String      -- Representa uma operação com erro e contém uma mensagem explicativa
    deriving (Show, Read, Eq, Generic)




-- --------Tipo LogEntry

-- Registra as entradas de log, possuindo informações
-- sobre uma operação realizada no sistema.
data LogEntry = LogEntry {
    timestamp :: UTCTime,  -- Data e hora em que a operação ocorreu
    acao      :: AcaoLog,  -- Tipo de ação (Add, Remove, Update, etc.)
    detalhes  :: String,   -- Descrição textual da operação
    status    :: StatusLog -- Resultado da operação (Sucesso ou Falha)
}deriving (Show, Read, Eq, Generic)




-- ----------------------------- Funções Puras de Transação -----------------------------

-- ----------Tipo ResultadoOperacao: tipo auxiliar

-- É o resultado de uma função de transação:
--   novo inventário + entrada de log gerada.
type ResultadoOperacao = (Inventario, LogEntry)




-- ----------Função addItem
-- Adiciona um novo item ao inventário.
-- Recebe como argumentos:
--   UTCTime  -> horário da operação
--   Item     -> o item a ser adicionado
--   Inventario -> o estado atual do inventário
-- Retorna: Either String ResultadoOperacao (erro ou sucesso)

addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem time item inv =
    if Map.member (itemID item) inv
        then 
            -- Caso o item já exista, cria uma entrada de log de falha.
            let logFail = LogEntry time Add
                        ("Falha ao adicionar: ID " ++ itemID item ++ " já existe.")
                        (Falha "ID duplicado")
            in Left "ID duplicado"
        else 
            -- Caso contrário, insere o item e gera um log de sucesso.
            let novoInv = Map.insert (itemID item) item inv
                logOk = LogEntry time Add
                    ("Item " ++ nome item ++ " adicionado com sucesso.")
                    Sucesso
            in Right (novoInv, logOk)
            
            
            
            
            
-- --------- Função removeItem

-- Remove um item existente do inventário e retorna erro caso o ID não exista.
-- Recebe: UTCTime, ID do item e o inventário atual.

removeItem :: UTCTime -> String -> Inventario -> Either String ResultadoOperacao
removeItem time idItem inv =
    case Map.lookup idItem inv of
        Nothing ->
            -- Caso o item não seja encontrado:
            let logFail = LogEntry time Remove
                    ("Falha ao remover: ID " ++ idItem ++ " não encontrado.")
                    (Falha "Item inexistente")
            in Left "Item inexistente"

        Just item ->
            -- Caso encontrado: remove o item e gera log de sucesso.
            let novoInv = Map.delete idItem inv
                logOk = LogEntry time Remove
                    ("Item " ++ nome item ++ " removido com sucesso.")
                    Sucesso
            in Right (novoInv, logOk)
            
            
            
            
            
-- -------- Função updateQty

-- Atualiza a quantidade de um item no inventário.
-- Retorna erro se o item não existir ou se a nova quantidade for negativa.

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty time idItem novaQtd inv =
    case Map.lookup idItem inv of
        Nothing ->
            -- Item inexistente:
            let logFail = LogEntry time Update
                    ("Falha ao atualizar: ID " ++ idItem ++ " não encontrado.")
                    (Falha "Item inexistente")
            in Left "Item inexistente"

        Just item ->
            if novaQtd < 0
                then 
                    -- Quantidade negativa é inválida:
                    let logFail = LogEntry time Update
                            ("Falha ao atualizar " ++ nome item ++ ": quantidade inválida.")
                            (Falha "Quantidade negativa")
                    in Left "Quantidade negativa"
                else 
                    -- Atualiza o item no inventário:
                    let itemAtualizado = item { quantidade = novaQtd }
                        novoInv = Map.insert idItem itemAtualizado inv
                        logOk = LogEntry time Update
                            ("Quantidade de " ++ nome item ++ " atualizada para " ++ show novaQtd ++ ".")
                            Sucesso
                    in Right (novoInv, logOk)
                    
                    
                    
                    
-- --------------------------------Main e Loop de Estado------------------------------

-- --------------------------- Caminhos de Arquivos ---------------------------
inventarioFile :: FilePath
inventarioFile = "Inventario.dat" -- Arquivo que guarda o inventário serializado

logFile :: FilePath
logFile = "Auditoria.log" -- Arquivo que armazena o log/auditoria do sistema




-- ----------- Função carregarInventario (CORRIGIDA E DETALHADA) -----------
-- Essa função garante que o inventário sempre seja carregado com segurança.
-- Caso o arquivo esteja vazio, corrompido ou não exista, o sistema cria um inventário limpo.
carregarInventario :: IO Inventario
carregarInventario = do
    -- Tenta ler o conteúdo do arquivo inventário
    conteudo <- catch (readFile inventarioFile) handler
    if null conteudo
        then do
            -- Caso esteja vazio, avisa o usuário e inicia um inventário vazio
            putStrLn "(Aviso) Inventário vazio ou inexistente. Criando novo inventário..."
            return Map.empty
        else 
            -- Caso exista conteúdo, tenta fazer o parsing com segurança
            catch (evaluate (read conteudo)) erroLeitura
  where
    handler :: IOException -> IO String
    handler _ = return "" -- Se o arquivo não existir, retorna string vazia
    erroLeitura :: IOException -> IO Inventario
    erroLeitura _ = do
        putStrLn "(Aviso) Erro ao ler o arquivo de inventário. Recriando inventário limpo..."
        return Map.empty




-- ----------- Função carregarLog (CORRIGIDA E DETALHADA) -----------
-- Essa função garante que o arquivo de log sempre exista e esteja pronto para uso.
-- Se estiver vazio ou corrompido, ele é recriado automaticamente.
carregarLog :: IO ()
carregarLog = catch
    (do
        conteudo <- readFile logFile
        if null conteudo
            then do
                putStrLn "(Aviso) Arquivo de log vazio. Criando novo..."
                writeFile logFile ""
            else return ()
    )
    (\(_ :: IOException) -> do
        putStrLn "(Aviso) Arquivo de log não encontrado. Criando novo..."
        writeFile logFile ""
    )




-- ----------- Função salvarInventario -----------
-- Serializa e salva o inventário atualizado em disco
salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile inventarioFile (show inv)




-- ----------- Função salvarLog -----------
-- Adiciona uma entrada de log (serializada) no final do arquivo de auditoria
salvarLog :: LogEntry -> IO ()
salvarLog logEntry = appendFile logFile (show logEntry ++ "\n")




-- ---------------------------- FUNÇÕES DE RELATÓRIO ----------------------------

-- ----------- Carrega todos os logs registrados até o momento -----------
carregarLogs :: IO [LogEntry]
carregarLogs = do
    conteudo <- catch (readFile logFile) handler
    if null conteudo then return [] else return (map read (lines conteudo))
  where
    handler :: IOException -> IO String
    handler _ = return ""




-- ----------- Filtra somente os logs que resultaram em falha -----------
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\logEntry -> case status logEntry of
                                    Falha _ -> True
                                    _       -> False)




-- ----------- Retorna o histórico de operações relacionadas a um item específico -----------
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idItem = filter (\logEntry -> idItem `elem` words (detalhes logEntry))




-- ----------- Calcula o item mais movimentado do log -----------
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
    let palavras = concatMap (words . detalhes) logs
        ids = filter (\x -> length x > 2 && all (`notElem` ".:,") x) palavras
        contagem :: Map String Int
        contagem = Map.fromListWith (+) [(x, 1) | x <- ids]
    in if Map.null contagem
        then Nothing
        else
            let pares = Map.toList contagem
                melhor = foldl1 (\acc x -> if snd x > snd acc then x else acc) pares
            in Just melhor




-- ----------- Exibe relatório completo no terminal -----------
exibirRelatorio :: IO ()
exibirRelatorio = do
    logs <- carregarLogs
    putStrLn "\n================== RELATÓRIO =================="
    putStrLn ("Total de registros de log: " ++ show (length logs))
    putStrLn "----------------------------------------------"
    putStrLn "Erros registrados:"
    mapM_ (putStrLn . detalhes) (logsDeErro logs)
    putStrLn "----------------------------------------------"
    case itemMaisMovimentado logs of
        Nothing -> putStrLn "Nenhum item movimentado ainda."
        Just (idItem, n) -> putStrLn ("Item mais movimentado: " ++ idItem ++ " (" ++ show n ++ " ocorrências)")
    putStrLn "==============================================\n"




-- ------------------ Parser e Execução de Comandos ------------------

-- processarComando recebe:
--   UTCTime  -> momento atual (para o log)
--   [String] -> comando tokenizado (ex.: words linha)
--   Inventario -> estado atual
-- retorna: IO Inventario (novo estado, possivelmente igual ao anterior)

processarComando :: UTCTime -> [String] -> Inventario -> IO Inventario

-- Caso o usuário não insira comandos
processarComando _ [] inv = do
    putStrLn "Comando vazio."
    return inv




-- -------------------- ADD --------------------
processarComando time ("add":idItem:nome:qtdStr:cat:_) inv =
    case reads qtdStr :: [(Int, String)] of
        [(qtd, "")] -> do
            let item = Item idItem nome qtd cat
            case addItem time item inv of
                Left msg -> do
                    let logFail = LogEntry time Add msg (Falha msg)
                    salvarLog logFail
                    putStrLn ("Erro: " ++ msg)
                    return inv
                Right (novoInv, logOk) -> do
                    salvarInventario novoInv
                    salvarLog logOk
                    putStrLn "Item adicionado com sucesso!"
                    return novoInv
        _ -> do
            let msg = "Quantidade inválida."
            let logFail = LogEntry time Add msg (Falha msg)
            salvarLog logFail
            putStrLn msg
            return inv




-- -------------------- REMOVE --------------------
processarComando time ("remove":idItem:_) inv =
    case removeItem time idItem inv of
        Left msg -> do
            let logFail = LogEntry time Remove msg (Falha msg)
            salvarLog logFail
            putStrLn ("Erro: " ++ msg)
            return inv
        Right (novoInv, logOk) -> do
            salvarInventario novoInv
            salvarLog logOk
            putStrLn "Item removido com sucesso!"
            return novoInv




-- -------------------- UPDATE --------------------
processarComando time ("update":idItem:novaQtdStr:_) inv =
    case reads novaQtdStr :: [(Int, String)] of
        [(novaQtd, "")] -> do
            case updateQty time idItem novaQtd inv of
                Left msg -> do
                    let logFail = LogEntry time Update msg (Falha msg)
                    salvarLog logFail
                    putStrLn ("Erro: " ++ msg)
                    return inv
                Right (novoInv, logOk) -> do
                    salvarInventario novoInv
                    salvarLog logOk
                    putStrLn "Quantidade atualizada!"
                    return novoInv
        _ -> do
            let msg = "Quantidade inválida."
            let logFail = LogEntry time Update msg (Falha msg)
            salvarLog logFail
            putStrLn msg
            return inv




-- -------------------- REPORT --------------------
processarComando _ ["report"] inv = do
    exibirRelatorio
    return inv




-- -------------------- EXIT --------------------
processarComando _ ["exit"] inv = do
    putStrLn "Encerrando programa..."
    return inv




-- -------------------- ETC --------------------
processarComando _ _ inv = do
    let msg = "Comando inválido."
    time <- getCurrentTime
    let logFail = LogEntry time QueryFail msg (Falha msg)
    salvarLog logFail
    putStrLn msg
    return inv




-- --------------------------------Loop Principal ------------------------------
loopPrincipal :: Inventario -> IO ()
loopPrincipal inv = do
    putStr "\n-  "
    hFlush stdout
    linha <- getLine
    if linha == "exit"
        then putStrLn "Saindo..."
        else do
            time <- getCurrentTime
            novoInv <- processarComando time (words linha) inv
            loopPrincipal novoInv




-- ---------- Ponto de entrada Main ------------
main :: IO ()
main = do
    putStrLn "======================================="
    putStrLn "   Sistema de Inventário - Haskell  "
    putStrLn "======================================="
    putStrLn " Atenção!! os nomes dos produtos NÃO podem conter espaço!"
    putStrLn "\nComandos disponíveis:"
    putStrLn " add <id> <nome> <qtd> <cat>"
    putStrLn " remove <id>"
    putStrLn " update <id> <qtd>"
    putStrLn " report"
    putStrLn " exit"
    putStrLn "----------------------------------------"

    carregarLog
    inventario <- carregarInventario
    loopPrincipal inventario
