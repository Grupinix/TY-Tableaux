# TY-Tableaux

Trabalho de Haskell feito por Thiago Mozart e Yuri Nogueira

## Como compilar

Projeto feito utilizando Haskell 8, a compilação do projeto deve ser feita utilizando o seguinte comando:

> make compile

## Modo de utilizar

O padrão de input deve ser em notação pré-fixada. Seguindo a seguinte lista de representações:

- Implicação: >
- Equivalência: -
- Negação: !
- E: ^
- Ou: v

### Exemplos:

Execute o comando `./haskell-tableaux` e depois insira a fórmula.

#### 1:
`./haskell-tableaux`
> '^(>(a,b),^(a,!b))'

O exemplo acima é a representação de:

> (a -> b) ^ (a ^ !b)

#### 2:
`./haskell-tableaux`
> 'v(>(a,b),-(a,b))'

O exemplo acima é a representação de:

> (a -> b) v (a <-> b)

#### 3:
`./haskell-tableaux`
> '^(a,!a)'

O exemplo acima é a representação de:

> (a ^ !a)

### 4:
`./haskell-tableaux`
> '>(v(p,^(q,r)),^(v(p,q),v(p,r)))'

O exemplo acima é a representação de:
> ((p v (q ^ r)) -> ((p v q) ^ (p v r)))

### 5:
`./haskell-tableaux`
> '>(b,^(a,v(b,a)))'

O exemplo acima é a representação de:
> (b -> (a ^ (b v a)))
