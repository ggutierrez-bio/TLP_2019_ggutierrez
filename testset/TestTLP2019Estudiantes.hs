module TestTLP2019Estudiantes where

import TLP2019
import CodeDataTypesTLP2019

testTLP2019 = putStr (concat (map executeTest tests))

executeTest (origen,destino,ndt,nds)
  | errores == "" = cabecera ++ "superado.\n"
  | otherwise     = cabecera ++ "NO SUPERADO:\n" ++ errores
    where cabecera = "Test \"" ++ origen ++ "\" -> \"" ++ destino ++ "\" "
          errores = errorNDT ++ errorNDS
          errorNDT
            | or (map ((\x -> x /= ndt).length) respuesta) = "-No todas las soluciones tienen el número de transiciones correcto.\n"
            | otherwise = ""
          errorNDS
            | length respuesta /= nds = "-El número de soluciones encontrado difiere del correcto.\n"
            | otherwise = ""
          respuesta = obtenerTransiciones origen destino

tests :: [(String,String,Int,Int)]
tests = [("casa","arbol",5,11),
         ("esternocleidomastoideo","semicircunferencia",19,180),
         ("anuario","aniversario",5,5),
         ("copito","nieve",6,15),
         ("saludo","saldo",1,1),
         ("dextrosa","sacarosa",4,1)
        ]
