import PdePreludat
import Library
import Test.Hspec


programaSuma :: Programa
programaSuma = add' . lodv 22 . swap . lodv 10

programaDivision :: Programa
programaDivision = division' . lod 1 . swap . lod 2 . str 2 0 . str 1 2

main :: IO ()
main = hspec $ do
  describe "Tests entrega 1 TP" $ do
    it "Programa suma 22 y 10 y acumulador A queda en 32" $ do
      (acumuladorA $ programaSuma xt8088) `shouldBe` 32
    it "Programa divide 2 por 0 y etiqueta queda con el error 'DIVISION BY ZERO'" $ do
      (etiqueta $ programaDivision xt8088) `shouldBe` "DIVISION BY ZERO"
    it "Programa divide 2 por 0 y etiqueta queda con error" $ do
      (etiqueta $ programaDivision xt8088) `shouldSatisfy` ((==16).length)
    it "Programa suma 22 y 10 y acumulador A queda en 32" $ do
      programaSuma xt8088 `shouldBe` Micro { acumuladorA = 32, acumuladorB = 0, etiqueta = "", memoria = [], programCounter = 4}

