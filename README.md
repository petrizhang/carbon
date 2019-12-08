<div align=center>
<img src="https://raw.githubusercontent.com/pzque/coco/master/doc/coco-logo-v3.png?token=ACTGKUY5Q75KUH2ICECRF5S56TJUM" width="300" alt="[coco logo]"/>
</div>

# Coco
A purely functional and strongly typed programming language running on JVM.

## Getting Stared

### Basic Value Types
```haskell
a :: Boolean
a = true

b :: Int
b = 1

c :: Float
c = 1.0

d :: Char        
d = 'c'  

e :: String     
e = "string"

f :: [Int]
f = [1, 2, 3]  

g :: (Int, String, Float) 
g = (1, "2", 3.0)

h :: {String: Int}
h = {       
  "a": 1,
  "b": 2
  "c": 3
}             

i :: {String}
i = {1, 2, 3}
```

### Function
```haskell
f :: a -> a -> a
f x y = x + y 

sum :: Int -> Int
sum n = if n == 1 then 1 else n + sum (n-1)

add :: Int -> Int
add = \x -> x + 1
```

## ADT
```haskell

```

