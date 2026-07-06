# -*- mode: janet; -*-
# Prime numbers computed various ways

(import deft)

# Simple trial-division primes
(defn primes
  "Returns a list of prime numbers less than n."
  [n]
  (def result @[])
  (for i 2 n
    (var prime? true)
    (each p result
      (when (zero? (% i p))
        (set prime? false)
        (break)))
    (when prime? (array/push result i)))
  result)

(printf "primes < 30: %n" (primes 30))

# Sieve of Eratosthenes
(defn sieve
  "Return primes up to n using Sieve of Eratosthenes."
  [n]
  (var candidates (range 2 n))
  (def result @[])
  (while (not (empty? candidates))
    (def p (first candidates))
    (array/push result p)
    (set candidates (filter (fn [x] (not= 0 (% x p))) (array/slice candidates 1))))
  result)

(printf "sieve <= 30: %n" (sieve 30))


# Using deft types

# define a predicate that returns true if a number is prime
(deft/define prime?
  [n :number] :boolean
   (and (> n 1)
        (or (= n 2)
            (and (odd? n)
                 (all (fn [d] (not= 0 (mod n d)))
                      (range 3 (inc (math/sqrt n)) 2))))))

# define a new type using the prime? predicate
(deft/deftype :prime prime?)

# define a simple (yet inefficient) function to find the nth prime below 10^5
(deft/define nth-prime [n :number]
   (get (filter |(deft/isa? $ :prime)
                (range 2 1e5))
        (- n 1)))

(print " 15th prime: " (nth-prime 15))  # 47
(print "101st prime: " (nth-prime 101)) # 547
(print "999th prime: " (nth-prime 999)) # 7907
