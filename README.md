# Shared Piggy Bank

This repo houses a simple Plutus Contract that acts as a shared piggy bank.

## Setting up

### Cabal+Nix build

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

Otherwise, you can use [direnv](https://github.com/direnv/direnv) which allows you to use your preferred shell. Once installed, just run:

```
$ echo "use nix" > .envrc # Or manually add "use nix" in .envrc if you already have one
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build server` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

## The Plutus Application Backend (PAB)

With the PAB we can serve and interact with contracts over a web API.
You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with one contract, the `PiggyBank` contract from `./src/Plutus/Contracts/PiggyBank.hs`.

Here's an example of running and interacting with this contract via the API. For this it will help if you have `jq` installed.

1. Compile the contract and build the server:

```
cabal build ppb
```

2. Run the PAB binary:

```
cabal exec -- ppb
````

This will then start up the server on port 9080.

3. Check what contracts are present:

```
curl -s http://localhost:9080/api/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

We're interested in the `ParameterisedPiggyBankContract`.

## Run the PAB executable
### Rule of this piggy bank 
The piggy bank has a restriction that you cannot withdraw unless it has accumulated at least 1 million lovelace.

### Put, empty and inspect

After starting the PAB, you may want to run `./run.sh` to send some requests to the server.

First, we activate four distinct wallets.

Then in order this following requests and responses can be observed

- Wallet 1 puts 199999 lovelace
- Wallet 2 puts 299999 lovelace
- Wallet 3 tries to empty but not allowed since total < 1 M lovelace
- Wallet 2 puts 1000000 lovelace.
- Wallet 4 inspect the wallet with 'inspect' endpoint. It finds 1499998 lovelace in total
- Wallet 4 tries to empty and is successful


Note that:
Wallet 3's failed attempt actually didn't cost it anything.

A strong selling point for Cardano is that we can check quite a bit of information before data ever hits the chain.
Anyone who spent hundreds of USD on a few occassions on a failed ETH transaction will appreciate this.
