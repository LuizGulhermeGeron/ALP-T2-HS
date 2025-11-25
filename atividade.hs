------------------------ Definição do tipo de dados ------------------------
-- Tipo de Dado Algébrico para representar expressões matemáticas
data Expressao = 
    Const Int
    | Var String
    | Soma Expressao Expressao
    | Produto Expressao Expressao
    | Potencia Expressao Int
    deriving(Show, Eq)


------------------------ Função de pré-processamento ------------------------
-- Adiciona espaços ao redor de parênteses para facilitar a tokenização
adicionaEspacos :: Char -> [Char]
adicionaEspacos c = if c `elem` "()"
                    then " " ++ [c] ++ " "
                    else [c]

------------------------ Função de tokenização ------------------------
-- Espera uma expressão pré-fixa como string e retorna uma lista dos símbolos

tokenize :: String -> [String]
tokenize s = words (concatMap adicionaEspacos s)

------------------------ Função de parsing ------------------------
-- Retorna a expressão e os tokens restantes
parse :: [[Char]] -> (Expressao, [[Char]])

parse ("(":tokens) =
    let 
        (op:tokens1) = tokens
    in 
        case op of
            "+" ->
                let 
                    (expr1, tokens2) = parse tokens1
                    (expr2, tokens3) = parse tokens2
                in 
                    (Soma expr1 expr2, tail tokens3)
            
            "*" ->
                let 
                    (expr1, tokens2) = parse tokens1
                    (expr2, tokens3) = parse tokens2
                in 
                    (Produto expr1 expr2, tail tokens3)

            "^" -> -- É uma potência
               let 
                    (expr1, tokens2) = parse tokens1
                   -- O segundo operando é um Int
                    (Const n, tokens3) = parse tokens2 -- Simplificação: assumindo n é Const
               in 
                    --(Potencia expr1 (round n), tail tokens3) -- tail consome o ")"
                    (Potencia expr1 n, tail tokens3) -- tail consome o ")"

            _   -> error "Operador desconhecido"

parse (token:resto) =
    case reads token :: [(Int, String)] of
        -- Tenta ler como um número
        [(val, "")] -> (Const val, resto)
        -- Se falhar, é uma variável
        _           -> (Var token, resto)


------------------------ Função de diferenciação ------------------------
-- Recebe uma expressão e a variável em relação à qual se deve derivar
-- Retorna a expressão derivada

-- comentei pq elas nao tao deixando compilar ja que nao estao implementadas
--derivar :: Expressao -> String -> Expressao

--------------Simplificação--------------

--simplificar :: Expressao -> Expressao


-------------Imprimir Infixa----------------

--imprimir :: Expressao -> [Char]

---------------------------------------

-- No GHCi:
-- let prefixa = "(^ (+ x 1) 2)"
-- let parse_out  = parse (tokenize prefixa)
-- let expr = fst parse_out
-- simplificar (derivar expr "x")

expToString :: Expressao -> String
expToString (Const n) = show n -- converte um unico numero p texto
expToString (Var x)   = x -- retorna a mesma variavel
expToString (Soma e1 e2) =
    "(" ++ expToString e1 ++ " + " ++ expToString e2 ++ ")"
expToString (Produto e1 e2) =
    "(" ++ expToString e1 ++ " * " ++ expToString e2 ++ ")"
expToString (Potencia e n) =
    "(" ++ expToString e ++ " ^ " ++ show n ++ ")"
    
main :: IO ()
main = do
    let expressao = Soma (Const 3) (Produto (Const 2) (Var "x")) -- o exemplo q ta no pdf
    let expressao2 = Const 1
    let expressao3 = Var "10"
    let expressao4 = Potencia (Const 1) 50
    let resultado = expToString expressao
    let resultado2 = expToString expressao2
    let resultado3 = expToString expressao3
    let resultado4 = expToString expressao4
    print resultado
    print resultado2
    print resultado3
    print resultado4
