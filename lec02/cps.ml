let rec fac n =
  if n = 0 then 1
  else n * fac (n - 1)

let rec fac' n k =
  if n = 0 then k 1
  else fac' (n - 1) (fun x -> k (n * x))

let test1 = fac' 3 (fun x -> x) = 6
let test2 = fac' 1 (fun x -> x) = 1
let test3 = fac' 5 (fun x -> x) = 120            


let rec fib n =
  if n < 2 then n
  else fib (n - 1) + fib (n - 2)

let rec fib' n k =
  if n < 2 then k n
  else fib' (n - 1) (fun x -> fib' (n - 2) (fun y -> k (x + y)))

let test1 = fib' 1 (fun x -> x) = 1
let test2 = fib' 5 (fun x -> x) = 5
let test3 = fib' 8 (fun x -> x) = 21            
