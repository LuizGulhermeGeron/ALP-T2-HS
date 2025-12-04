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

derivar (Const n) d = Const 0
derivar (Var x) d
    | x == d = Const 1
    | otherwise = Const 0
derivar (Soma a b) d = Soma (derivar a d) (derivar b d)
derivar (Produto a b) d = Soma (Produto a (derivar b d)) (Produto b (derivar a d))
derivar (Potencia a n) d = Produto (Produto (Const n) (Potencia a (n-1))) (derivar a d)

--------------Simplificação--------------

simplificar :: Expressao -> Expressao
simplificar (Soma a b) = simplificarB (Soma (simplificar a) (simplificar b))
simplificar (Produto a b) = simplificarB (Produto (simplificar a) (simplificar b))
simplificar (Potencia a n) = simplificarB (Potencia (simplificar a) n)
simplificar a = a

simplificarB :: Expressao -> Expressao
-- soma de dois números em duas somas
simplificarB (Soma (Const n1) (Soma (Const n2) a)) = simplificarB(Soma (Const (n1 + n2)) a)
simplificarB (Soma (Const n1) (Soma a (Const n2))) = simplificarB(Soma (Const (n1 + n2)) a)
simplificarB (Soma (Soma (Const n2) a) (Const n1)) = simplificarB(Soma (Const (n1 + n2)) a)
simplificarB (Soma (Soma a (Const n2)) (Const n1)) = simplificarB(Soma (Const (n1 + n2)) a)
-- Produto de dois números em dois produtos
simplificarB (Produto (Const n1) (Produto (Const n2) a)) = simplificarB(Produto (Const (n1 * n2)) a)
simplificarB (Produto (Const n1) (Produto a (Const n2))) = simplificarB(Produto (Const (n1 * n2)) a)
simplificarB (Produto (Produto (Const n2) a) (Const n1)) = simplificarB(Produto (Const (n1 * n2)) a)
simplificarB (Produto (Produto a (Const n2)) (Const n1)) = simplificarB(Produto (Const (n1 * n2)) a)
-- soma com 0
simplificarB (Soma a (Const 0)) = a
simplificarB (Soma (Const 0) a) = a
-- produto com 0
simplificarB (Produto a (Const 0)) = Const 0
simplificarB (Produto (Const 0) a) = Const 0
-- produto com 1
simplificarB (Produto a (Const 1)) = a
simplificarB (Produto (Const 1) a) = a
-- soma de dois números
simplificarB (Soma (Const n1) (Const n2)) = Const (n1 + n2)
-- produto de dois números
simplificarB (Produto (Const n1) (Const n2)) = Const (n1 * n2)
-- potência de 0
simplificarB (Potencia a 0) = Const 1
-- potência de 1
simplificarB (Potencia a 1) = a
-- número e variável
simplificarB a = a

-------------Imprimir Infixa----------------

imprimir :: Expressao -> [Char]
imprimir (Const n) = show n
imprimir (Var x) = x
imprimir (Soma a b) = "(" ++ (imprimir a) ++ " + " ++ (imprimir b) ++ ")"
imprimir (Produto a b) = "(" ++ (imprimir a) ++ " * " ++ (imprimir b) ++ ")"
imprimir (Potencia a n) = "(" ++ (imprimir a) ++ " ^ " ++ (show n) ++ ")"

---------------------------------------

-- No GHCi:
-- let prefixa = "(^ (+ x 1) 2)"
-- let parse_out  = parse (tokenize prefixa)
-- let expr = fst parse_out
-- simplificar (derivar expr "x")
    
main :: IO ()
main = do
    let teste = Soma (Produto (Const 3) (Potencia (Var "x") 2)) (Produto (Const 2) (Var "x")) 
    let expressao = Soma (Const 3) (Produto (Const 2) (Var "x")) -- o exemplo q ta no pdf
    let expressao2 = Const 1
    let expressao3 = Var "10"
    let expressao4 = Potencia (Const 1) 50

    let expressoes = [teste, expressao, expressao2, expressao3, expressao4]
    
    mapM_ (\teste -> do
        print ("Expressão: " ++ imprimir teste)
        print ("Expressão derivada: " ++ imprimir (derivar teste "x"))
        print ("Expressão derivada simplificada: " ++ imprimir (simplificar (derivar teste "x")))
        putStrLn ""
        ) expressoes


