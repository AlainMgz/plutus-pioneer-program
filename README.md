# Plutus Pioneer Program

## Lectures

- [Lecture #1](https://youtu.be/IEn6jUo-0vU)

  - Welcome
  - The (E)UTxO-model
  - Running an example auction contract on a local Playground
  - Homework

- [Lecture #2](https://youtu.be/E5KRk5y9KjQ)

  - Triggering change.
  - Low-level, untyped on-chain validation scripts.
  - High-level, typed on-chain validation scripts.

- [Lecture #3](https://youtu.be/Lk1eIVm_ZTQ)

  - Script context.
  - Time handling.
  - Parameterized contracts.

- [Lecture #4](https://youtu.be/6Reuh0xZDjY)

  - Monads
  - The `EmulatorTrace` monad.
  - The `Contract` monad.

## Code Examples

- Lecture #1: [English Auction](code/week01)
- Lecture #2: [Simple Validation](code/week02)
- Lecture #3: [Validation Context & Parameterized Contracts](code/week03)
- Lecture #4: [Monads, `EmulatorTrace` & `Contract`](code/week04)

## Exercises

- Week #1

  - Build the [English Auction](code/week01) contract with `cabal build` (you may need to run `cabal update` first). (DONE)
  - Clone the [The Plutus repository](https://github.com/input-output-hk/plutus), check out the correct commit
    as specified in [cabal.project](code/week01/cabal.project). (DONE)
  - Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, STOP and fix the cache." (DONE)
  - Enter a `nix-shell`. (DONE)
  - Go to the `plutus-playground-client` folder. (DONE)
  - Start the Playground server with `plutus-playground-server`. (DONE)
  - Start the Playground client (in another `nix-shell`) with `npm run start`. (DONE)
  - Copy-paste the auction contract into the Playground editor - don't forget to remove the module header! (DONE)
  - Compile. (DONE)
  - Simulate various auction scenarios. (DONE)

- Week #2

  - Fix and complete the code in the [Homework1](code/week02/src/Week02/Homework1.hs) module. (DONE)
  - Fix and complete the code in the [Homework2](code/week02/src/Week02/Homework2.hs) module. (DONE)

- Week #3

  - Fix and complete the code in the [Homework1](code/week03/src/Week03/Homework1.hs) module. (DONE)
  - Fix and complete the code in the [Homework2](code/week03/src/Week03/Homework2.hs) module. (DONE)

- Week #4

  - Write an appropriate `EmulatorTrace` that uses the `payContract` contract in the [Homework](code/week04/src/Week04/Homework.hs) module.
  - Catch errors in the `payContract` contract in the same module.

## Solutions

- Week #2

  - [`Homework1`](code/week02/src/Week02/Solution1.hs)
  - [`Homework2`](code/week02/src/Week02/Solution2.hs)

- Week #3

  - [`Homework1`](code/week03/src/Week03/Solution1.hs)
  - [`Homework2`](code/week03/src/Week03/Solution2.hs)

## Some Plutus Modules

- [`Plutus.Trace.Emulator`](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs), contains types and functions related to traces.
- [`Plutus.V1.Ledger.Contexts`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs), contains the definition of the context-related types.
- [`Plutus.V1.Ledger.Interval`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Interval.hs), contains the definition of and helper functions for the `Interval` type.
- [`Plutus.V1.Ledger.Slot`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Slot.hs), contains the definition of the `Slot` type.
- [`PlutusTx.Data`](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Data.hs), contains the definition of the `Data` type.
- [`PlutusTx.IsData.Class`](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/IsData/Class.hs), defines the `IsData` class.

## Plutus community playground

- [Week 1 Community Playground(Legacy)](https://playground-week1.plutus-community.com/)
- [Week 2 Community Playground(Legacy)](https://playground-week2.plutus-community.com/)
- [Week 3 Community Playground(Current)](https://playground.plutus-community.com/)


## Additional Resources

- [The Plutus repository](https://github.com/input-output-hk/plutus)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell & Cryptocurrencies course Mongolia](https://www.youtube.com/playlist?list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm)




