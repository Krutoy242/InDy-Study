import InDyModel

flip_pipe = flip pipe

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = pipe flip_pipe flip_pipe

(.::) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.::) = pipe (.:) flip_pipe

rotate_2_3 :: (a -> b -> c -> d) -> a -> c -> b -> d
rotate_2_3 = apply flip_pipe flip

-- Haskell Library composition robinstar
robinstar :: (a -> b -> c -> d) -> c -> a -> b -> d
robinstar = flip . (flip .)

rotate_3_1_2 :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate_3_1_2 = apply rotate_2_3 rotate_2_3

-- Rotated pipe2
pipe2' :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
pipe2' = apply rotate_3_1_2 pipe2

-- Pipe3
--pipe3 :: (a -> b -> c -> d) -> (x -> a) -> (x -> b) -> (x -> c) -> x -> d





greater = (>)
greater' = flip greater
greater_10 = greater' 10
my_predicate :: Int -> Bool
my_predicate = greater_10

true_value :: Int -> Int
true_value = (+) 100

false_value :: Int -> Int
false_value x = x + 1


f1 = apply3 pipe2' op_choice my_predicate true_value
z  = apply3 pipe2' id f1 false_value

q1 = pipe2' op_choice
q2 = pipe2' id
f1' = apply2 q1 my_predicate true_value
f2' = apply2 q2 f1' false_value

apply2' = flip apply2
f2''    = apply2' f1' q2 false_value

z1 = apply3 pipe2' id (apply3 pipe2' op_choice my_predicate true_value) false_value

pipe3 = ((pipe2' id .) .) . pipe2'

main :: IO()
main = do
    print ("hi")
    print (map z [1, 2, 20])