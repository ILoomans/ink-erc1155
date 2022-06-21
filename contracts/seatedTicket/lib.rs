#![cfg_attr(not(feature = "std"), no_std)]

use ink_lang as ink;

#[ink::contract]
mod erc20 {
    use ink_storage::{
        lazy::{
            Lazy
            },
        collections::HashMap as StorageHashMap,
        

        // traits::SpreadAllocate,
    };
    use ink_prelude::string::String;
    use ink_prelude::vec::Vec;
    use ink_prelude::collections::BTreeMap;
    use redeemables::Redeemables;
    use ink_env::call::FromAccountId;

    /// A simple ERC-20 contract.
    #[ink(storage)]
    // #[derive(SpreadAllocate)]
    pub struct Erc20 {
        /// Total token supply.
        total_supply: Lazy<Balance>,
        /// Mapping from owner to number of owned token.
        balances: BTreeMap<AccountId, Balance>,
        /// Mapping of the token amount which an account is allowed to withdraw
        /// from another account.
        allowances: BTreeMap<(AccountId, AccountId), Balance>,
        price: u128,
        owner:AccountId,
        contract_balance: Balance,
        proof_key: StorageHashMap<AccountId,Vec<u8>>,    
        verifier: StorageHashMap<AccountId,bool>  ,
        seats: Vec<String>,
        seat_taken: StorageHashMap<String,bool>,
        seat_balance:StorageHashMap<AccountId,bool>,
        has_seats: bool,
    }

    /// Event emitted when a token transfer occurs.
    #[ink(event)]
    pub struct Transfer {
        #[ink(topic)]
        from: Option<AccountId>,
        #[ink(topic)]
        to: Option<AccountId>,
        value: Balance,
    }

    /// Event emitted when an approval occurs that `spender` is allowed to withdraw
    /// up to the amount of `value` tokens from `owner`.
    #[ink(event)]
    pub struct Approval {
        #[ink(topic)]
        owner: AccountId,
        #[ink(topic)]
        spender: AccountId,
        value: Balance,
    }

    /// The ERC-20 error types.
    #[derive(Debug, PartialEq, Eq, scale::Encode, scale::Decode)]
    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
    pub enum Error {
        /// Returned if not enough balance to fulfill a request is available.
        InsufficientBalance,
        /// Returned if not enough allowance to fulfill a request is available.
        InsufficientAllowance,
        /// Incorrect price
        IncorrectPrice,
        /// Not owner
        NotOwner,
        /// Not Verifier
        NotVerifier,
        /// Cannot fetch
        CannotFetch,
        /// Seat Taken
        SeatTaken,
        /// Mismatch between seats and amount of tickets
        SeatMismatch
    }

    /// The ERC-20 result type.
    pub type Result<T> = core::result::Result<T, Error>;

    impl Erc20 {
        /// Creates a new ERC-20 contract with the specified initial supply.
        #[ink(constructor)]
        pub fn new(initial_supply: Balance, price: u128, owner: AccountId,seats:Vec<String>,date:u64,end_date_address:AccountId) -> Self {
            let mut has_seats = true;
            if seats.clone().len()==0{
                has_seats = false;
            }

            let mut end_date_contract: Redeemables = FromAccountId::from_account_id(end_date_address);
            end_date_contract.set_end_date(date);
            let caller = Self::env().caller();
                let mut instance = Self {
                    total_supply: Lazy::new(initial_supply),
                    balances: Default::default(),
                    allowances: Default::default(),
                    price,
                    owner,
                    contract_balance:0,
                    proof_key: Default::default(),
                    verifier: Default::default(),
                    seats,
                    seat_taken: Default::default(),
                    seat_balance: Default::default(),
                    has_seats,
                };
            instance.balances.insert(owner, initial_supply);
            Lazy::set(&mut instance.total_supply, initial_supply);
            Self::env().emit_event(Transfer {
                from: None,
                to: Some(owner),
                value: initial_supply,
            });
            return instance
        }




        // /// Default initializes the ERC-20 contract with the specified initial supply.
        // fn new_init(&mut self, initial_supply: Balance, price: u128, owner: AccountId,seats:Vec<String>) {

        // }

        /// Returns the total token supply.
        #[ink(message)]
        pub fn total_supply(&self) -> Balance {
            *self.total_supply
        }

        #[ink(message)]
        pub fn contract_balance(&self) -> Balance {
            return self.contract_balance
        }

        #[ink(message)]
        pub fn proof(&self, to: AccountId) -> Vec<u8> {
            return self.proof_key.get(&to).unwrap().clone()
        }

        #[ink(message)]
        pub fn get_owner(&self) -> AccountId {
            return self.owner;
        }


        #[ink(message)]
        pub fn is_verifier(&self,to: AccountId) -> bool {
            *self.verifier.get(&to).unwrap_or(&false)
        } 


        #[ink(message)]
        pub fn get_price(&self) -> u128 {
            return self.price;
        }
 
        
        /// Returns the account balance for the specified `owner`.
        ///
        /// Returns `0` if the account is non-existent.
        #[ink(message)]
        pub fn balance_of(&self, owner: AccountId) -> Balance {
            self.balance_of_impl(owner)
        }

        /// Returns the account balance for the specified `owner`.
        ///
        /// Returns `0` if the account is non-existent.
        ///
        /// # Note
        ///
        /// Prefer to call this method over `balance_of` since this
        /// works using references which are more efficient in Wasm.
        #[inline]
        fn balance_of_impl(&self, owner: AccountId) -> Balance {
            *self.balances.get(&owner).unwrap_or(&0)
        }

        /// Returns the amount which `spender` is still allowed to withdraw from `owner`.
        ///
        /// Returns `0` if no allowance has been set.
        #[ink(message)]
        pub fn allowance(&self, owner: AccountId, spender: AccountId) -> Balance {
            self.allowance_impl(owner, spender)
        }

        /// Returns the amount which `spender` is still allowed to withdraw from `owner`.
        ///
        /// Returns `0` if no allowance has been set.
        ///
        /// # Note
        ///
        /// Prefer to call this method over `allowance` since this
        /// works using references which are more efficient in Wasm.
        #[inline]
        fn allowance_impl(&self, owner: AccountId, spender: AccountId) -> Balance {
            *self.allowances.get(&(owner, spender)).unwrap_or(&0)
        }


        #[ink(message)]
        pub fn add_verifier(&mut self, to: AccountId) -> Result<()> {
            let from = self.env().caller();
            if from == self.owner {
                self.verifier.insert(to,true);
                Ok(())
            }else{
                return Err(Error::NotOwner);

            }
        }


        /// Transfers `value` amount of tokens from the caller's account to account `to`.
        ///
        /// On success a `Transfer` event is emitted.
        ///
        /// # Errors
        ///
        /// Returns `InsufficientBalance` error if there are not enough tokens on
        /// the caller's account balance.
        #[ink(message)]
        pub fn transfer(&mut self, to: AccountId, value: Balance) -> Result<()> {
            let from = self.env().caller();
            self.transfer_from_to(&from, &to, value)
        }


                // check how this work, do we actually need check2 or does the function end when we return the error
                #[ink(message,payable)]
                pub fn purchase_tickets(&mut self,to: AccountId, value: Balance, signature: Vec<u8>, seats: Vec<String>) -> Result<()> {
                    self.proof_key.insert(to,signature);
        
        
                    if self.price*value != self.env().transferred_balance() {
                        return Err(Error::IncorrectPrice);
                    }else{
        
                        if(self.has_seats==true){
                        if !self.is_seat_available(seats.clone()) {
                                return Err(Error::SeatTaken)
                        }
                        
                        if value==seats.clone().len() as u128{
                        for f in seats.clone().iter(){
                            self.seat_taken.insert(f.clone(),true);
                        }
                    }
                    // add tokens to balance
        
                    self.transfer_from_to(&self.get_owner(),&to, value);
                    self.contract_balance += self.env().transferred_balance();
                        Ok(())
        
                    
        
                        }else{
                         return Err(Error::SeatMismatch)
                         }
                    }
                }
        
                #[ink(message)]
                pub fn is_seat_available(&mut self, seats:Vec<String>) -> bool {
                    for f in seats.iter() {
                        if *self.seat_taken.get(f).unwrap_or(&true) {
                            return false
                        }
                    }
                    return true;
                }
        

                       // little comment
        #[ink(message,payable)]
        pub fn clear(&mut self) -> Result<()> {
            let check = self.env().caller();
            if check != self.owner {
                //fix this error
                return Err(Error::IncorrectPrice);
            }else{
                // dont have to check this
            self.env().transfer(self.owner,self.contract_balance);
            self.contract_balance = 0;
            Ok(()) 
            }

        }
 
        /// Allows `spender` to withdraw from the caller's account multiple times, up to
        /// the `value` amount.
        ///
        /// If this function is called again it overwrites the current allowance with `value`.
        ///
        /// An `Approval` event is emitted.
        #[ink(message)]
        pub fn approve(&mut self, spender: AccountId, value: Balance) -> Result<()> {
            let owner = self.env().caller();
            self.allowances.insert((owner, spender), value);
            self.env().emit_event(Approval {
                owner,
                spender,
                value,
            });
            Ok(())
        }

        /// Transfers `value` tokens on the behalf of `from` to the account `to`.
        ///
        /// This can be used to allow a contract to transfer tokens on ones behalf and/or
        /// to charge fees in sub-currencies, for example.
        ///
        /// On success a `Transfer` event is emitted.
        ///
        /// # Errors
        ///
        /// Returns `InsufficientAllowance` error if there are not enough tokens allowed
        /// for the caller to withdraw from `from`.
        ///
        /// Returns `InsufficientBalance` error if there are not enough tokens on
        /// the account balance of `from`.
        #[ink(message)]
        pub fn transfer_from(
            &mut self,
            from: AccountId,
            to: AccountId,
            value: Balance,
        ) -> Result<()> {
            let caller = self.env().caller();
            let allowance = self.allowance_impl(from, caller);
            if allowance < value {
                return Err(Error::InsufficientAllowance)
            }
            self.transfer_from_to(&from, &to, value)?;
            self.allowances
                .insert((from, caller), (allowance - value));
            Ok(())
        }


        #[ink(message)]
        pub fn burn(&mut self, from: AccountId, value: Balance) -> Result<()> {
            let caller = self.env().caller();
            let is_ver = *self.verifier.get(&caller).unwrap_or(&false);
            if is_ver == true {
            let allowance = self.balances.get(&from).unwrap_or(&0);
            if allowance < &value {
                return Err(Error::InsufficientAllowance)
            }else{
                self.balances.insert(from, (allowance - value));
                Ok(())
            }
            } else {
                return Err(Error::NotVerifier);
            }
        }


        /// Transfers `value` amount of tokens from the caller's account to account `to`.
        ///
        /// On success a `Transfer` event is emitted.
        ///
        /// # Errors
        ///
        /// Returns `InsufficientBalance` error if there are not enough tokens on
        /// the caller's account balance.
        fn transfer_from_to(
            &mut self,
            from: &AccountId,
            to: &AccountId,
            value: Balance,
        ) -> Result<()> {
            let from_balance = self.balance_of_impl(*from);
            if from_balance < value {
                return Err(Error::InsufficientBalance)
            }

            self.balances.insert(*from, (from_balance - value));
            let to_balance = self.balance_of_impl(*to);
            self.balances.insert(*to, (to_balance + value));
            self.env().emit_event(Transfer {
                from: Some(*from),
                to: Some(*to),
                value,
            });
            Ok(())
        }
    }

    /// Unit tests.
    #[cfg(not(feature = "ink-experimental-engine"))]
    #[cfg(test)]
    mod tests {
        /// Imports all the definitions from the outer scope so we can use them here.
        use super::*;

        type Event = <Erc20 as ::ink_lang::reflect::ContractEventBase>::Type;

        use ink_lang as ink;

        fn assert_transfer_event(
            event: &ink_env::test::EmittedEvent,
            expected_from: Option<AccountId>,
            expected_to: Option<AccountId>,
            expected_value: Balance,
        ) {
            let decoded_event = <Event as scale::Decode>::decode(&mut &event.data[..])
                .expect("encountered invalid contract event data buffer");
            if let Event::Transfer(Transfer { from, to, value }) = decoded_event {
                assert_eq!(from, expected_from, "encountered invalid Transfer.from");
                assert_eq!(to, expected_to, "encountered invalid Transfer.to");
                assert_eq!(value, expected_value, "encountered invalid Trasfer.value");
            } else {
                panic!("encountered unexpected event kind: expected a Transfer event")
            }
            let expected_topics = vec![
                encoded_into_hash(&PrefixedValue {
                    value: b"Erc20::Transfer",
                    prefix: b"",
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Erc20::Transfer::from",
                    value: &expected_from,
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Erc20::Transfer::to",
                    value: &expected_to,
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Erc20::Transfer::value",
                    value: &expected_value,
                }),
            ];
            for (n, (actual_topic, expected_topic)) in
                event.topics.iter().zip(expected_topics).enumerate()
            {
                let topic = actual_topic
                    .decode::<Hash>()
                    .expect("encountered invalid topic encoding");
                assert_eq!(topic, expected_topic, "encountered invalid topic at {}", n);
            }
        }

        /// The default constructor does its job.
        #[ink::test]
        fn new_works() {
            // Constructor works.
            let _erc20 = Erc20::new(100);

            // Transfer event triggered during initial construction.
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(1, emitted_events.len());

            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
        }

        /// The total supply was applied.
        #[ink::test]
        fn total_supply_works() {
            // Constructor works.
            let erc20 = Erc20::new(100);
            // Transfer event triggered during initial construction.
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            // Get the token total supply.
            assert_eq!(erc20.total_supply(), 100);
        }

        /// Get the actual balance of an account.
        #[ink::test]
        fn balance_of_works() {
            // Constructor works
            let erc20 = Erc20::new(100);
            // Transfer event triggered during initial construction
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>()
                    .expect("Cannot get accounts");
            // Alice owns all the tokens on contract instantiation
            assert_eq!(erc20.balance_of(accounts.alice), 100);
            // Bob does not owns tokens
            assert_eq!(erc20.balance_of(accounts.bob), 0);
        }

        #[ink::test]
        fn transfer_works() {
            // Constructor works.
            let mut erc20 = Erc20::new(100);
            // Transfer event triggered during initial construction.
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>()
                    .expect("Cannot get accounts");

            assert_eq!(erc20.balance_of(accounts.bob), 0);
            // Alice transfers 10 tokens to Bob.
            assert_eq!(erc20.transfer(accounts.bob, 10), Ok(()));
            // Bob owns 10 tokens.
            assert_eq!(erc20.balance_of(accounts.bob), 10);

            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 2);
            // Check first transfer event related to ERC-20 instantiation.
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            // Check the second transfer event relating to the actual trasfer.
            assert_transfer_event(
                &emitted_events[1],
                Some(AccountId::from([0x01; 32])),
                Some(AccountId::from([0x02; 32])),
                10,
            );
        }

        #[ink::test]
        fn invalid_transfer_should_fail() {
            // Constructor works.
            let mut erc20 = Erc20::new(100);
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>()
                    .expect("Cannot get accounts");

            assert_eq!(erc20.balance_of(accounts.bob), 0);
            // Get contract address.
            let callee = ink_env::account_id::<ink_env::DefaultEnvironment>();
            // Create call
            let mut data =
                ink_env::test::CallData::new(ink_env::call::Selector::new([0x00; 4])); // balance_of
            data.push_arg(&accounts.bob);
            // Push the new execution context to set Bob as caller
            ink_env::test::push_execution_context::<ink_env::DefaultEnvironment>(
                accounts.bob,
                callee,
                1000000,
                1000000,
                data,
            );

            // Bob fails to transfers 10 tokens to Eve.
            assert_eq!(
                erc20.transfer(accounts.eve, 10),
                Err(Error::InsufficientBalance)
            );
            // Alice owns all the tokens.
            assert_eq!(erc20.balance_of(accounts.alice), 100);
            assert_eq!(erc20.balance_of(accounts.bob), 0);
            assert_eq!(erc20.balance_of(accounts.eve), 0);

            // Transfer event triggered during initial construction.
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 1);
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
        }

        #[ink::test]
        fn transfer_from_works() {
            // Constructor works.
            let mut erc20 = Erc20::new(100);
            // Transfer event triggered during initial construction.
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>()
                    .expect("Cannot get accounts");

            // Bob fails to transfer tokens owned by Alice.
            assert_eq!(
                erc20.transfer_from(accounts.alice, accounts.eve, 10),
                Err(Error::InsufficientAllowance)
            );
            // Alice approves Bob for token transfers on her behalf.
            assert_eq!(erc20.approve(accounts.bob, 10), Ok(()));

            // The approve event takes place.
            assert_eq!(ink_env::test::recorded_events().count(), 2);

            // Get contract address.
            let callee = ink_env::account_id::<ink_env::DefaultEnvironment>();
            // Create call.
            let mut data =
                ink_env::test::CallData::new(ink_env::call::Selector::new([0x00; 4])); // balance_of
            data.push_arg(&accounts.bob);
            // Push the new execution context to set Bob as caller.
            ink_env::test::push_execution_context::<ink_env::DefaultEnvironment>(
                accounts.bob,
                callee,
                1000000,
                1000000,
                data,
            );

            // Bob transfers tokens from Alice to Eve.
            assert_eq!(
                erc20.transfer_from(accounts.alice, accounts.eve, 10),
                Ok(())
            );
            // Eve owns tokens.
            assert_eq!(erc20.balance_of(accounts.eve), 10);

            // Check all transfer events that happened during the previous calls:
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 3);
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            // The second event `emitted_events[1]` is an Approve event that we skip checking.
            assert_transfer_event(
                &emitted_events[2],
                Some(AccountId::from([0x01; 32])),
                Some(AccountId::from([0x05; 32])),
                10,
            );
        }

        #[ink::test]
        fn allowance_must_not_change_on_failed_transfer() {
            let mut erc20 = Erc20::new(100);
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>()
                    .expect("Cannot get accounts");

            // Alice approves Bob for token transfers on her behalf.
            let alice_balance = erc20.balance_of(accounts.alice);
            let initial_allowance = alice_balance + 2;
            assert_eq!(erc20.approve(accounts.bob, initial_allowance), Ok(()));

            // Get contract address.
            let callee = ink_env::account_id::<ink_env::DefaultEnvironment>();
            // Create call.
            let mut data =
                ink_env::test::CallData::new(ink_env::call::Selector::new([0x00; 4])); // balance_of
            data.push_arg(&accounts.bob);
            // Push the new execution context to set Bob as caller.
            ink_env::test::push_execution_context::<ink_env::DefaultEnvironment>(
                accounts.bob,
                callee,
                1000000,
                1000000,
                data,
            );

            // Bob tries to transfer tokens from Alice to Eve.
            let emitted_events_before = ink_env::test::recorded_events();
            assert_eq!(
                erc20.transfer_from(accounts.alice, accounts.eve, alice_balance + 1),
                Err(Error::InsufficientBalance)
            );
            // Allowance must have stayed the same
            assert_eq!(
                erc20.allowance(accounts.alice, accounts.bob),
                initial_allowance
            );
            // No more events must have been emitted
            let emitted_events_after = ink_env::test::recorded_events();
            assert_eq!(emitted_events_before.count(), emitted_events_after.count());
        }
    }

    /// For calculating the event topic hash.
    struct PrefixedValue<'a, 'b, T> {
        pub prefix: &'a [u8],
        pub value: &'b T,
    }

    impl<X> scale::Encode for PrefixedValue<'_, '_, X>
    where
        X: scale::Encode,
    {
        #[inline]
        fn size_hint(&self) -> usize {
            self.prefix.size_hint() + self.value.size_hint()
        }

        #[inline]
        fn encode_to<T: scale::Output + ?Sized>(&self, dest: &mut T) {
            self.prefix.encode_to(dest);
            self.value.encode_to(dest);
        }
    }

    #[cfg(feature = "ink-experimental-engine")]
    #[cfg(test)]
    mod tests_experimental_engine {
        use super::*;

        use ink_env::Clear;
        use ink_lang as ink;

        type Event = <Erc20 as ::ink_lang::reflect::ContractEventBase>::Type;

        fn assert_transfer_event(
            event: &ink_env::test::EmittedEvent,
            expected_from: Option<AccountId>,
            expected_to: Option<AccountId>,
            expected_value: Balance,
        ) {
            let decoded_event = <Event as scale::Decode>::decode(&mut &event.data[..])
                .expect("encountered invalid contract event data buffer");
            if let Event::Transfer(Transfer { from, to, value }) = decoded_event {
                assert_eq!(from, expected_from, "encountered invalid Transfer.from");
                assert_eq!(to, expected_to, "encountered invalid Transfer.to");
                assert_eq!(value, expected_value, "encountered invalid Trasfer.value");
            } else {
                panic!("encountered unexpected event kind: expected a Transfer event")
            }
            let expected_topics = vec![
                encoded_into_hash(&PrefixedValue {
                    value: b"Erc20::Transfer",
                    prefix: b"",
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Erc20::Transfer::from",
                    value: &expected_from,
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Erc20::Transfer::to",
                    value: &expected_to,
                }),
                encoded_into_hash(&PrefixedValue {
                    prefix: b"Erc20::Transfer::value",
                    value: &expected_value,
                }),
            ];

            let topics = event.topics.clone();
            for (n, (actual_topic, expected_topic)) in
                topics.iter().zip(expected_topics).enumerate()
            {
                let mut topic_hash = Hash::clear();
                let len = actual_topic.len();
                topic_hash.as_mut()[0..len].copy_from_slice(&actual_topic[0..len]);

                assert_eq!(
                    topic_hash, expected_topic,
                    "encountered invalid topic at {}",
                    n
                );
            }
        }

        /// The default constructor does its job.
        #[ink::test]
        fn new_works() {
            // Constructor works.
            let _erc20 = Erc20::new(100);

            // Transfer event triggered during initial construction.
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(1, emitted_events.len());

            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
        }

        /// The total supply was applied.
        #[ink::test]
        fn total_supply_works() {
            // Constructor works.
            let erc20 = Erc20::new(100);
            // Transfer event triggered during initial construction.
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            // Get the token total supply.
            assert_eq!(erc20.total_supply(), 100);
        }

        /// Get the actual balance of an account.
        #[ink::test]
        fn balance_of_works() {
            // Constructor works
            let erc20 = Erc20::new(100);
            // Transfer event triggered during initial construction
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>();
            // Alice owns all the tokens on contract instantiation
            assert_eq!(erc20.balance_of(accounts.alice), 100);
            // Bob does not owns tokens
            assert_eq!(erc20.balance_of(accounts.bob), 0);
        }

        #[ink::test]
        fn transfer_works() {
            // Constructor works.
            let mut erc20 = Erc20::new(100);
            // Transfer event triggered during initial construction.
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>();

            assert_eq!(erc20.balance_of(accounts.bob), 0);
            // Alice transfers 10 tokens to Bob.
            assert_eq!(erc20.transfer(accounts.bob, 10), Ok(()));
            // Bob owns 10 tokens.
            assert_eq!(erc20.balance_of(accounts.bob), 10);

            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 2);
            // Check first transfer event related to ERC-20 instantiation.
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            // Check the second transfer event relating to the actual trasfer.
            assert_transfer_event(
                &emitted_events[1],
                Some(AccountId::from([0x01; 32])),
                Some(AccountId::from([0x02; 32])),
                10,
            );
        }

        #[ink::test]
        fn invalid_transfer_should_fail() {
            // Constructor works.
            let mut erc20 = Erc20::new(100);
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>();

            assert_eq!(erc20.balance_of(accounts.bob), 0);

            // Set the contract as callee and Bob as caller.
            let contract = ink_env::account_id::<ink_env::DefaultEnvironment>();
            ink_env::test::set_callee::<ink_env::DefaultEnvironment>(contract);
            ink_env::test::set_caller::<ink_env::DefaultEnvironment>(accounts.bob);

            // Bob fails to transfers 10 tokens to Eve.
            assert_eq!(
                erc20.transfer(accounts.eve, 10),
                Err(Error::InsufficientBalance)
            );
            // Alice owns all the tokens.
            assert_eq!(erc20.balance_of(accounts.alice), 100);
            assert_eq!(erc20.balance_of(accounts.bob), 0);
            assert_eq!(erc20.balance_of(accounts.eve), 0);

            // Transfer event triggered during initial construction.
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 1);
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
        }

        #[ink::test]
        fn transfer_from_works() {
            // Constructor works.
            let mut erc20 = Erc20::new(100);
            // Transfer event triggered during initial construction.
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>();

            // Bob fails to transfer tokens owned by Alice.
            assert_eq!(
                erc20.transfer_from(accounts.alice, accounts.eve, 10),
                Err(Error::InsufficientAllowance)
            );
            // Alice approves Bob for token transfers on her behalf.
            assert_eq!(erc20.approve(accounts.bob, 10), Ok(()));

            // The approve event takes place.
            assert_eq!(ink_env::test::recorded_events().count(), 2);

            // Set the contract as callee and Bob as caller.
            let contract = ink_env::account_id::<ink_env::DefaultEnvironment>();
            ink_env::test::set_callee::<ink_env::DefaultEnvironment>(contract);
            ink_env::test::set_caller::<ink_env::DefaultEnvironment>(accounts.bob);

            // Bob transfers tokens from Alice to Eve.
            assert_eq!(
                erc20.transfer_from(accounts.alice, accounts.eve, 10),
                Ok(())
            );
            // Eve owns tokens.
            assert_eq!(erc20.balance_of(accounts.eve), 10);

            // Check all transfer events that happened during the previous calls:
            let emitted_events = ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events.len(), 3);
            assert_transfer_event(
                &emitted_events[0],
                None,
                Some(AccountId::from([0x01; 32])),
                100,
            );
            // The second event `emitted_events[1]` is an Approve event that we skip checking.
            assert_transfer_event(
                &emitted_events[2],
                Some(AccountId::from([0x01; 32])),
                Some(AccountId::from([0x05; 32])),
                10,
            );
        }

        #[ink::test]
        fn allowance_must_not_change_on_failed_transfer() {
            let mut erc20 = Erc20::new(100);
            let accounts =
                ink_env::test::default_accounts::<ink_env::DefaultEnvironment>();

            // Alice approves Bob for token transfers on her behalf.
            let alice_balance = erc20.balance_of(accounts.alice);
            let initial_allowance = alice_balance + 2;
            assert_eq!(erc20.approve(accounts.bob, initial_allowance), Ok(()));

            // Get contract address.
            let callee = ink_env::account_id::<ink_env::DefaultEnvironment>();
            ink_env::test::set_callee::<ink_env::DefaultEnvironment>(callee);
            ink_env::test::set_caller::<ink_env::DefaultEnvironment>(accounts.bob);

            // Bob tries to transfer tokens from Alice to Eve.
            let emitted_events_before =
                ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(
                erc20.transfer_from(accounts.alice, accounts.eve, alice_balance + 1),
                Err(Error::InsufficientBalance)
            );
            // Allowance must have stayed the same
            assert_eq!(
                erc20.allowance(accounts.alice, accounts.bob),
                initial_allowance
            );
            // No more events must have been emitted
            let emitted_events_after =
                ink_env::test::recorded_events().collect::<Vec<_>>();
            assert_eq!(emitted_events_before.len(), emitted_events_after.len());
        }
    }

    #[cfg(test)]
    fn encoded_into_hash<T>(entity: &T) -> Hash
    where
        T: scale::Encode,
    {
        use ink_env::{
            hash::{
                Blake2x256,
                CryptoHash,
                HashOutput,
            },
            Clear,
        };
        let mut result = Hash::clear();
        let len_result = result.as_ref().len();
        let encoded = entity.encode();
        let len_encoded = encoded.len();
        if len_encoded <= len_result {
            result.as_mut()[..len_encoded].copy_from_slice(&encoded);
            return result
        }
        let mut hash_output = <<Blake2x256 as HashOutput>::Type as Default>::default();
        <Blake2x256 as CryptoHash>::hash(&encoded, &mut hash_output);
        let copy_len = core::cmp::min(hash_output.len(), len_result);
        result.as_mut()[0..copy_len].copy_from_slice(&hash_output[0..copy_len]);
        result
    }
}