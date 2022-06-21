cd compiled 
rm -rf *
cd ..
cd contracts/redeemables
cargo +nightly contract build
mv target/ink/redeemables.wasm ../../compiled
mv target/ink/redeemables.contract ../../compiled
cd ..
cd collectables
cargo +nightly contract build
mv target/ink/collectables.wasm ../../compiled
mv target/ink/collectables.contract ../../compiled
cd ..
cd generalTicket
cargo +nightly contract build
mv target/ink/generalTicket.wasm ../../compiled
mv target/ink/generalTicket.contract ../../compiled
cd ..
cd seatedTicket
cargo +nightly contract build
mv target/ink/seatedTicket.wasm ../../compiled
mv target/ink/seatedTicket.contract ../../compiled
cd ..
cd ..

