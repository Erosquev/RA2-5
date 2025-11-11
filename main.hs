{-# LANGUAGE DeriveGeneric #-}
-- Extensão de linguagem que permite derivar automaticamente a classe 'Generic'.
-- Isso é útil quando queremos compatibilidade com bibliotecas de serialização,

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import GHC.Generics (Generic)


-- -------------------------Tipos principais do sistema de inventário ----------------------

-- -------Representa um item individual no inventário
data Item = Item {
    itemID    :: String,   
    nome      :: String,   
    quantidade :: Int,     
    categoria :: String    
}

-- aqui aplico as implementações necessárias:
--   Show: permite converter o item em texto (para imprimir com 'print')
--   Read: permite ler um item a partir de texto (com 'read')
--   Eq: permite comparar dois itens (==)
--   Generic: habilita serialização automática (ex: JSON)
deriving (Show, Read, Eq, Generic)



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
}
deriving (Show, Read, Eq, Generic) -- As mesmas implemetações são aplicadas aqui:


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
            
