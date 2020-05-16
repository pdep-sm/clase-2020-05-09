import PdePreludat
import Library
import Test.Hspec

programaAvancePC :: Programa
programaAvancePC = nop . nop . nop

programaSuma :: Programa
programaSuma = add' . lodv 22 . swap . lodv 10

programaDivision :: Programa
programaDivision = division' . lod 1 . swap . lod 2 . str 2 0 . str 1 2

fp20 = xt8088 {
  acumuladorA = 7,
  acumuladorB = 24
}

main :: IO ()
main = hspec $ do
  describe "Tests entrega 1 TP" $ do
    it "Programa con 3 NOP sólo cambia el PC a 3" $ do
      programaAvancePC  xt8088 `shouldBe` xt8088 { programCounter = 3 }
    it "LODV cambia acumulador A" $ do
      (acumuladorA . lodv 5) xt8088 `shouldBe` 5
    it "LODV no cambia acumulador B" $ do
      (acumuladorB . lodv 5) xt8088 `shouldBe` 0
    it "SWAP deja en acumulador A el valor del B" $ do
      (acumuladorA . swap) fp20 `shouldBe` 24
    it "SWAP deja en acumulador B el valor del A" $ do
      (acumuladorB . swap) fp20 `shouldBe` 7
    it "Programa suma 22 y 10 y acumulador A queda en 32" $ do
      (acumuladorA $ programaSuma xt8088) `shouldBe` 32
    it "Programa divide 2 por 0 y etiqueta queda con el error 'DIVISION BY ZERO'" $ do
      (etiqueta $ programaDivision xt8088) `shouldBe` "DIVISION BY ZERO"
    it "Programa divide 2 por 0 y etiqueta queda con error" $ do
      (etiqueta $ programaDivision xt8088) `shouldSatisfy` ((==16).length)
    it "Programa suma 22 y 10 y acumulador A queda en 32" $ do
      programaSuma xt8088 `shouldBe` Micro { acumuladorA = 32, acumuladorB = 0, etiqueta = "", memoria = [], programCounter = 4}

