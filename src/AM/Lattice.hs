module AM.Lattice where

class Lattice a where
    join :: a
    meet :: a
    lub :: a -> a -> a
    glb :: a -> a -> a