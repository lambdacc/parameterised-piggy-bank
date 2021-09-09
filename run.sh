#!/bin/bash
export MOM=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "ParameterisedPiggyBankContract", "caWallet":{"getWallet": 1}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export DAD=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "ParameterisedPiggyBankContract", "caWallet":{"getWallet": 2}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export Jack=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "ParameterisedPiggyBankContract", "caWallet":{"getWallet": 3}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export Jill=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "ParameterisedPiggyBankContract", "caWallet":{"getWallet": 4}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')

sleep 4
curl -H "Content-Type: application/json" -X POST -d '{"ppBeneficiary":"xyz","ppAmount":100000}' http://localhost:9080/api/contract/instance/$MOM/endpoint/put && sleep 4
curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/$Jack/endpoint/inspect && sleep 4
curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/$Jill/endpoint/inspect && sleep 4
curl -H "Content-Type: application/json" -X POST -d 200000 http://localhost:9080/api/contract/instance/$DAD/endpoint/put && sleep 4
curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/$Jill/endpoint/inspect && sleep 4

curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/$Jack/endpoint/empty && sleep 4
curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/$Jill/endpoint/empty && sleep 4

curl -H "Content-Type: application/json" -X POST -d 100000 http://localhost:9080/api/contract/instance/$MOM/endpoint/put && sleep 4
curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/$Jill/endpoint/inspect && sleep 4
curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/$Jill/endpoint/empty
