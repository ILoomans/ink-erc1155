#![cfg_attr(not(feature = "std"), no_std)]

pub use self::redeemables::Redeemables;
use ink_lang as ink;

// The ERC-1155 error types.

// pub type Result<T, E = Error> = Result<T, E>;
/// Holds a simple `i32` value that can be incremented and decremented.

#[ink::contract]
pub mod redeemables {
    use ink_storage::traits::{PackedLayout, SpreadLayout};

    #[derive(Debug, PartialEq, scale::Encode, scale::Decode)]
    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
    pub enum Error {
        /// The sender of the transaction is not the ERC1155 contract
        NotERC1155Contract,
        /// The sender of the transaction is not the owner
        NotOwner,
        /// The ERC1155 Contract has already been set
        ContractAlreadySet,
    }

    pub type Result<T> = core::result::Result<T, Error>;
    pub type TokenId = u128;
    // type Balance = <ink_env::DefaultEnvironment as ink_env::Environment>::Balance;

    #[derive(
        Debug, PartialEq, Eq, scale::Encode, scale::Decode, PackedLayout, SpreadLayout,
    )]
    #[cfg_attr(feature = "std", derive(::scale_info::TypeInfo))]
    pub struct Staking {
        pub id: TokenId,
        pub balance: Balance,
        pub staker: AccountId,
        pub setid: u32,
    }

    #[ink(storage)]
    pub struct Redeemables {
        value: i32,
        contract_owner: AccountId,
        erc1155_address: AccountId,
    }

    impl Redeemables {
        /// Initializes the value to the initial value.
        #[ink(constructor)]
        pub fn new(init_value: i32) -> Self {
            let contract_owner = Self::env().caller();
            Self {
                value: init_value,
                contract_owner,
                erc1155_address: Default::default(),
            }
        }

        #[ink(message)]
        pub fn get_contract_address(&mut self) -> AccountId {
            self.env().account_id()
        }

        #[ink(message)]
        pub fn stake_nft(&mut self) -> Result<()> {
            if self.env().caller() != self.erc1155_address {
                return Err(Error::NotERC1155Contract);
            }

            Ok(())
        }

        #[ink(message)]
        pub fn setErc1155Contract(&mut self, address: AccountId) -> Result<()> {
            if self.env().caller() != self.contract_owner {
                return Err(Error::NotOwner);
            }
            self.erc1155_address = address;
            Ok(())
        }
        /// Mutates the internal value.
        #[ink(message)]
        pub fn inc(&mut self, by: i32) {
            self.value += by;
        }

        /// Returns the current state.
        #[ink(message)]
        pub fn get(&self) -> i32 {
            self.value
        }
    }
}
// #![cfg_attr(not(feature = "std"), no_std)]

// pub use self::redeemables::Redeemables;
// use ink_lang as ink;

// #[ink::contract]
// pub mod redeemables {
//     /// Holds a simple `i32` value that can be incremented and decremented.
//     #[ink(storage)]
//     pub struct redeemables {
//         value: i32,
//     }

//     impl Redeemables {
//         /// Initializes the value to the initial value.
//         #[ink(constructor)]
//         pub fn new(init_value: i32) -> Self {
//             Self { value: init_value }
//         }

//         /// Mutates the internal value.
//         #[ink(message)]
//         pub fn inc(&mut self, by: i32) {
//             self.value += by;
//         }

//         /// Returns the current state.
//         #[ink(message)]
//         pub fn get(&self) -> i32 {
//             self.value
//         }
//     }
// }
