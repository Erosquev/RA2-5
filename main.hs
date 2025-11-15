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
    itemID    :: String,    -- Identificador único do item (chave no Map)
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
    | QueryFail     -- Quando uma consulta ou operação falha (ex: comando inválido)
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
    detalhes  :: String,   -- Descrição textual da operação (ex: qual item foi afetado)
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
--   UTCTime  -> horário da operação (para o log)
--   Item     -> o item a ser adicionado
--   Inventario -> o estado atual do inventário
-- Retorna: Either String ResultadoOperacao (erro ou sucesso)

-- Valida se o ID já existe. Caso exista, retorna erro (Left).
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao -- essa é a assinatura da função 
addItem time item inv =
    if Map.member (itemID item) inv -- Verifica se o itemID já existe no Map (inv é um Map String Item). Retorna True se o item já está cadastrado (executando o then) e False caso contrário (executando o else).
        then 
            let logFail = LogEntry time Add -- Cria uma entrada de log (logFail) com:
                        ("Falha ao adicionar: ID " ++ itemID item ++ " já existe.") -- Detalhe da falha
                        (Falha "ID duplicado") -- Status de falha
                in Left "ID duplicado" -- Retorna Left "ID duplicado" (ou seja, falha)


        else -- Se o ID não existe, Usa Map.insert para inserir o novo item no inventário
            let novoInv = Map.insert (itemID item) item inv -- Insere o novo item no inventário
                logOk = LogEntry time Add -- Cria um log de sucesso (logOk)
                    ("Item " ++ nome item ++ " adicionado com sucesso.") -- Detalhe do sucesso
                    Sucesso -- Status de sucesso
            in Right (novoInv, logOk) -- Retorna o novo estado do inventário e o log de sucesso
            
            
            
            
            
-- --------- Função removeItem

-- Remove uma quantidade de um item existente do inventário.
-- Retorna erro caso o ID não exista, a quantidade seja inválida ou o estoque seja insuficiente.
-- Recebe: UTCTime, ID do item, quantidade a remover e o inventário atual.

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time idItem qtdRemover inv =
    case Map.lookup idItem inv of
        Nothing ->
            -- ID não existe
            let msg = "Item inexistente"
                logFail = LogEntry time Remove msg (Falha msg)
            in Left msg

        Just item ->
            let estoqueAtual = quantidade item in

            if qtdRemover <= 0 then
                -- Quantidade de remoção inválida
                let msg = "Quantidade inválida para remoção."
                    logFail = LogEntry time Remove msg (Falha msg)
                in Left msg

            else if qtdRemover > estoqueAtual then
                -- Tentou remover mais do que existe (Estoque insuficiente)
                let msg = "Estoque insuficiente."
                    logFail = LogEntry time Remove msg (Falha msg)
                in Left msg

            else if qtdRemover == estoqueAtual then
                -- Remove completamente o item (quantidade = estoque atual)
                let novoInv = Map.delete idItem inv -- Remove a chave do Map
                    logOk = LogEntry time Remove
                                ("Item " ++ nome item ++ " removido completamente.")
                                Sucesso
                in Right (novoInv, logOk)

            else
                -- Remoção parcial -> reduz quantidade
                let novoItem = item { quantidade = estoqueAtual - qtdRemover } -- Cria um novo Item com a quantidade reduzida
                    novoInv  = Map.insert idItem novoItem inv -- Atualiza o Map com o novo Item
                    logOk = LogEntry time Remove
                                ("Removidas " ++ show qtdRemover ++
                                 " unidades de " ++ nome item ++ ".")
                                Sucesso
                in Right (novoInv, logOk)
            
            
            
            
            
-- -------- Função updateQty

-- Atualiza a quantidade de um item no inventário.
-- Retorna erro se o item não existir ou se a nova quantidade for negativa.
-- Recebe: UTCTime, ID do item, nova quantidade e o inventário atual.

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
                    let itemAtualizado = item { quantidade = novaQtd } -- Cria um novo Item com a nova quantidade
                        novoInv = Map.insert idItem itemAtualizado inv -- Atualiza o Map
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




-- ----------- Função carregarInventario -----------
-- Essa função garante que o inventário sempre seja carregado com segurança.
-- Caso o arquivo esteja vazio, corrompido ou não exista, o sistema cria um inventário limpo.
-- Utiliza 'catch' para tratar a exceção de arquivo não encontrado (requisito de robustez).
carregarInventario :: IO Inventario
carregarInventario = do
    -- Tenta ler o conteúdo do arquivo inventário
    conteudo <- catch (readFile inventarioFile) handler
    if null conteudo
        then do
            -- Caso esteja vazio ou o handler tenha retornado "" (arquivo não existe),
            -- avisa o usuário e inicia um inventário vazio (Map.empty).
            putStrLn "(Aviso) Inventário vazio ou inexistente. Criando novo inventário..."
            return Map.empty
        else 
            -- Caso exista conteúdo, tenta fazer o parsing com segurança (read)
            -- O 'evaluate' força a avaliação do 'read conteudo' para que o 'catch'
            -- possa capturar erros de parsing (arquivo corrompido).
            catch (evaluate (read conteudo)) erroLeitura
  where
    -- Handler para IOException (ex: arquivo não encontrado)
    handler :: IOException -> IO String
    handler _ = return "" -- Se o arquivo não existir, retorna string vazia para ser tratada acima
    -- Handler para erro de leitura/parsing (ex: arquivo corrompido)
    erroLeitura :: IOException -> IO Inventario
    erroLeitura _ = do
        putStrLn "(Aviso) Erro ao ler o arquivo de inventário. Recriando inventário limpo..."
        return Map.empty




-- ----------- Função carregarLog -----------
-- Essa função garante que o arquivo de log sempre exista e esteja pronto para uso.
-- Se estiver vazio ou corrompido, ele é recriado automaticamente.
-- Garante que o arquivo exista para que 'appendFile' não falhe.
carregarLog :: IO ()
carregarLog = catch
    (do
        conteudo <- readFile logFile
        if null conteudo
            then do
                putStrLn "(Aviso) Arquivo de log vazio. Criando novo..."
                writeFile logFile "" -- Cria o arquivo vazio se ele existir mas estiver vazio
            else return ()
    )
    (\(_ :: IOException) -> do
        putStrLn "(Aviso) Arquivo de log não encontrado. Criando novo..."
        writeFile logFile "" -- Cria o arquivo vazio se ele não existir
    )


-- ----------- Função salvarInventario -----------
-- Serializa o inventário atualizado (usando 'show') e salva em disco.
-- Utiliza 'writeFile', sobrescrevendo o conteúdo anterior (persistência de estado).
salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile inventarioFile (show inv)




-- ----------- Função salvarLog -----------
-- Adiciona uma entrada de log (serializada) no final do arquivo de auditoria.
-- Utiliza 'appendFile' para garantir o modo "append-only" (requisito de auditoria).
salvarLog :: LogEntry -> IO ()
salvarLog logEntry = appendFile logFile (show logEntry ++ "\n")




-- ---------------------------- FUNÇÕES DE RELATÓRIO (Puras) ----------------------------

-- ----------- Carrega todos os logs registrados até o momento -----------
-- Lê o arquivo de log, separa as linhas e tenta fazer o 'read' de cada LogEntry.
carregarLogs :: IO [LogEntry]
carregarLogs = do
    conteudo <- catch (readFile logFile) handler
    if null conteudo then return [] else return (map read (lines conteudo))
  where
    -- Trata a exceção de arquivo não encontrado
    handler :: IOException -> IO String
    handler _ = return ""


-- ----------- Filtra somente os logs que resultaram em falha -----------
-- Função pura que recebe uma lista de logs e retorna apenas aqueles com StatusLog = Falha.
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\logEntry -> case status logEntry of
                                    Falha _ -> True
                                    _       -> False)


-- ----------- Retorna o histórico de operações relacionadas a um item específico -----------
-- Função pura que filtra logs cujo campo 'detalhes' contenha o ID do item.
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idItem = filter (\logEntry -> idItem `elem` words (detalhes logEntry))


-- ----------- Calcula o item mais movimentado do log -----------
-- Função pura que analisa o campo 'detalhes' de todos os logs para contar a frequência de IDs.
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
    let palavras = concatMap (words . detalhes) logs -- Concatena todas as palavras dos detalhes
        -- Filtra as palavras para tentar isolar os IDs (heurística simples)
        ids = filter (\x -> length x > 2 && all (`notElem` ".:,") x) palavras
        -- Cria um Map de contagem de ocorrências de cada ID
        contagem :: Map String Int
        contagem = Map.fromListWith (+) [(x, 1) | x <- ids]
    in if Map.null contagem
        then Nothing -- Retorna Nothing se não houver logs
        else
            -- Encontra o par (ID, Contagem) com a maior contagem
            let pares = Map.toList contagem
                melhor = foldl1 (\acc x -> if snd x > snd acc then x else acc) pares
            in Just melhor


-- ----------- Exibe relatório completo no terminal -----------
-- Função de IO que carrega os logs e exibe os resultados das funções puras de análise.
exibirRelatorio :: IO ()
exibirRelatorio = do
    logs <- carregarLogs -- Carrega todos os logs
    putStrLn "\n================== RELATÓRIO =================="
    putStrLn ("Total de registros de log: " ++ show (length logs))
    putStrLn "----------------------------------------------"
    putStrLn "Erros registrados:"
    -- Exibe os detalhes de todos os logs de erro
    mapM_ (putStrLn . detalhes) (logsDeErro logs)
    putStrLn "----------------------------------------------"
    -- Exibe o item mais movimentado
    case itemMaisMovimentado logs of
        Nothing -> putStrLn "Nenhum item movimentado ainda."
        Just (idItem, n) -> putStrLn ("Item mais movimentado: " ++ idItem ++ " (" ++ show n ++ " ocorrências)")
    putStrLn "==============================================\n"


-- ------------------ Parser e Execução de Comandos (IO) ------------------

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
-- Comando: add <id> <nome> <qtd> <cat>
processarComando time ("add":idItem:nome:qtdStr:cat:_) inv =
    case reads qtdStr :: [(Int, String)] of -- Tenta converter a string de quantidade para Int
        [(qtd, "")] -> do -- Conversão bem-sucedida
            let item = Item idItem nome qtd cat
            case addItem time item inv of -- Chama a função pura de lógica
                Left msg -> do -- Falha na lógica (ex: ID duplicado)
                    let logFail = LogEntry time Add msg (Falha msg)
                    salvarLog logFail -- Salva apenas o log de falha
                    putStrLn ("Erro: " ++ msg)
                    return inv -- Retorna o inventário inalterado
                Right (novoInv, logOk) -> do -- Sucesso na lógica
                    salvarInventario novoInv -- Salva o novo estado do inventário
                    salvarLog logOk -- Salva o log de sucesso
                    putStrLn "Item adicionado com sucesso!"
                    return novoInv -- Retorna o novo inventário
        _ -> do -- Falha na conversão de quantidade
            let msg = "Quantidade inválida."
            let logFail = LogEntry time Add msg (Falha msg)
            salvarLog logFail
            putStrLn msg
            return inv


-- -------------------- REMOVE --------------------
-- Comando: remove <id> <quantidade>
processarComando time ("remove":idItem:qtdStr:_) inv =
    case reads qtdStr :: [(Int, String)] of -- Tenta converter a string de quantidade para Int
        [(qtd, "")] -> do -- Conversão bem-sucedida
            case removeItem time idItem qtd inv of -- Chama a função pura de lógica
                Left msg -> do -- Falha na lógica (ex: item inexistente, estoque insuficiente)
                    let logFail = LogEntry time Remove msg (Falha msg)
                    salvarLog logFail
                    putStrLn ("Erro: " ++ msg)
                    return inv
                Right (novoInv, logOk) -> do -- Sucesso na lógica
                    salvarInventario novoInv
                    salvarLog logOk
                    putStrLn "Remoção bem-sucedida!"
                    return novoInv
        _ -> do -- Falha na conversão de quantidade
            let msg = "Quantidade inválida."
            let logFail = LogEntry time Remove msg (Falha msg)
            salvarLog logFail
            putStrLn msg
            return inv


-- -------------------- UPDATE --------------------
-- Comando: update <id> <nova_quantidade>
processarComando time ("update":idItem:novaQtdStr:_) inv =
    case reads novaQtdStr :: [(Int, String)] of -- Tenta converter a string de nova quantidade para Int
        [(novaQtd, "")] -> do -- Conversão bem-sucedida
            case updateQty time idItem novaQtd inv of -- Chama a função pura de lógica
                Left msg -> do -- Falha na lógica (ex: item inexistente, quantidade negativa)
                    let logFail = LogEntry time Update msg (Falha msg)
                    salvarLog logFail
                    putStrLn ("Erro: " ++ msg)
                    return inv
                Right (novoInv, logOk) -> do -- Sucesso na lógica
                    salvarInventario novoInv
                    salvarLog logOk
                    putStrLn "Quantidade atualizada!"
                    return novoInv
        _ -> do -- Falha na conversão de quantidade
            let msg = "Quantidade inválida."
            let logFail = LogEntry time Update msg (Falha msg)
            salvarLog logFail
            putStrLn msg
            return inv


-- -------------------- REPORT --------------------
-- Comando: report (Gera e exibe o relatório de logs)
processarComando _ ["report"] inv = do
    exibirRelatorio -- Chama a função de IO para exibir o relatório
    return inv -- Retorna o inventário inalterado


-- -------------------- LIST --------------------
-- Comando: list (Exibe o inventário atual)
processarComando _ ["list"] inv = do
    if Map.null inv
        then putStrLn "Inventário vazio."
        else do
            putStrLn "\n=== Itens no inventário ==="
            mapM_ print (Map.elems inv) -- Imprime cada item no inventário
    return inv


-- -------------------- EXIT --------------------
-- Comando: exit (Encerra o loop principal)
processarComando _ ["exit"] inv = do
    putStrLn "Encerrando programa..."
    return inv -- Retorna o inventário, que será ignorado pelo loop principal


-- -------------------- ETC --------------------
-- Comando inválido ou com formato incorreto
processarComando _ _ inv = do
    let msg = "Comando inválido."
    time <- getCurrentTime
    -- Registra a falha no log como QueryFail
    let logFail = LogEntry time QueryFail msg (Falha msg)
    salvarLog logFail
    putStrLn msg
    return inv


-- --------------------------------Loop Principal (IO) ------------------------------
-- Função recursiva que mantém o estado do inventário e processa comandos.
loopPrincipal :: Inventario -> IO ()
loopPrincipal inv = do
    putStr "\n-  "
    hFlush stdout -- Garante que o prompt seja exibido imediatamente
    linha <- getLine -- Lê a entrada do usuário
    if linha == "exit"
        then putStrLn "Saindo..." -- Condição de parada
        else do
            time <- getCurrentTime -- Obtém o timestamp atual para o log
            novoInv <- processarComando time (words linha) inv -- Processa o comando
            loopPrincipal novoInv -- Chama o loop com o novo estado do inventário


-- ---------- Ponto de entrada Main (IO) ------------
-- Inicializa o sistema, carrega os dados e inicia o loop principal.
main :: IO ()
main = do
    putStrLn "======================================="
    putStrLn "   Sistema de Inventário - Haskell  "
    putStrLn "======================================="
    putStrLn " Atenção!! os nomes dos produtos NÃO podem conter espaço!"
    putStrLn "\nComandos disponíveis:"
    putStrLn " add <id> <nome> <qtd> <cat>"
    putStrLn " remove <id> <quantidade>"
    putStrLn " update <id> <qtd>"
    putStrLn " report"
    putStrLn " list"
    putStrLn " exit"
    putStrLn "----------------------------------------"

    carregarLog -- Garante que o arquivo de log exista
    inventario <- carregarInventario -- Carrega o estado inicial do inventário
    loopPrincipal inventario -- Inicia o loop de interação com o usuário
