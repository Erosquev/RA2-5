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
