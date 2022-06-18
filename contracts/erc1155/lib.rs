#![cfg_attr(not(feature = "std"), no_std)]

use ink_env::AccountId;
use ink_lang as ink;
use ink_prelude::string::String;
use ink_prelude::vec::Vec;
use ink_storage::traits::{PackedLayout, SpreadLayout};
use ink_storage::lazy::Lazy;
use ink_env::call::FromAccountId;

// This is the return value that we expect if a smart contract supports receiving ERC-1155
// tokens.
//
// It is calculated with
// `bytes4(keccak256("onERC1155Received(address,address,uint256,uint256,bytes)"))`, and corresponds
// to 0xf23a6e61.
#[cfg_attr(test, allow(dead_code))]
const ON_ERC_1155_RECEIVED_SELECTOR: [u8; 4] = [0xF2, 0x3A, 0x6E, 0x61];

// This is the return value that we expect if a smart contract supports batch receiving ERC-1155
// tokens.
//
// It is calculated with
// `bytes4(keccak256("onERC1155BatchReceived(address,address,uint256[],uint256[],bytes)"))`, and
// corresponds to 0xbc197c81.
const _ON_ERC_1155_BATCH_RECEIVED_SELECTOR: [u8; 4] = [0xBC, 0x19, 0x7C, 0x81];

/// A type representing the unique IDs of tokens managed by this contract.
pub type TokenId = u128;

type Balance = <ink_env::DefaultEnvironment as ink_env::Environment>::Balance;

#[derive(
    Debug, PartialEq, Eq, scale::Encode, scale::Decode, PackedLayout, SpreadLayout,
)]
#[cfg_attr(feature = "std", derive(::scale_info::TypeInfo))]
pub struct IssuerFeatures {
    pub name: String,
    pub status: bool,
}

#[derive(
    Debug, PartialEq, Eq, scale::Encode, scale::Decode, PackedLayout, SpreadLayout,
)]
#[cfg_attr(feature = "std", derive(::scale_info::TypeInfo))]
pub struct NFTSetFeatures {
    pub name: String,
    pub description: String,
    pub status: bool,
    pub owner: AccountId,
}

#[derive(
    Debug, PartialEq, Eq, scale::Encode, scale::Decode, PackedLayout, SpreadLayout,
)]
#[cfg_attr(feature = "std", derive(::scale_info::TypeInfo))]
pub struct TokenFeatures {
    pub name: String,
    pub image: String,
    pub issuer: AccountId,
}

// The ERC-1155 error types.
#[derive(Debug, PartialEq, scale::Encode, scale::Decode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub enum Error {
    /// This token ID has not yet been created by the contract.
    UnexistentToken,
    /// The caller tried to sending tokens to the zero-address (`0x00`).
    ZeroAddressTransfer,
    /// The caller is not approved to transfer tokens on behalf of the account.
    NotApproved,
    /// The account does not have enough funds to complete the transfer.
    InsufficientBalance,
    /// An account does not need to approve themselves to transfer tokens.
    SelfApproval,
    /// The number of tokens being transferred does not match the specified number of transfers.
    BatchTransferMismatch,
    /// Only the Contract Owner can perform this action.
    NotContractOwner,
    /// Only a registered Issuer can perform this action.
    NotIssuer,
    /// Set id exists.
    SetIdExists,
    /// Not allowed to perform this action
    NotAllowed,
    // Token already exists
    TokenExists,
    CannotFetchValue,
}

// The ERC-1155 result types.
pub type Result<T> = core::result::Result<T, Error>;

/// Evaluate `$x:expr` and if not true return `Err($y:expr)`.
///
/// Used as `ensure!(expression_to_ensure, expression_to_return_on_false)`.
macro_rules! ensure {
    ( $condition:expr, $error:expr $(,)? ) => {{
        if !$condition {
            return ::core::result::Result::Err(::core::convert::Into::into($error));
        }
    }};
}

/// The interface for an ERC-1155 compliant contract.
///
/// The interface is defined here: <https://eips.ethereum.org/EIPS/eip-1155>.
///
/// The goal of ERC-1155 is to allow a single deployed contract to manage a variety of assets.
/// These assets can be fungible, non-fungible, or a combination.
///
/// By tracking multiple assets the ERC-1155 standard is able to support batch transfers, which
/// make it easy to transfer a mix of multiple tokens at once.
#[ink::trait_definition]
pub trait Erc1155 {
    /// Transfer a `value` amount of `token_id` tokens to the `to` account from the `from`
    /// account.
    ///
    /// Note that the call does not have to originate from the `from` account, and may originate
    /// from any account which is approved to transfer `from`'s tokens.
    #[ink(message)]
    fn safe_transfer_from(
        &mut self,
        from: AccountId,
        to: AccountId,
        token_id: TokenId,
        value: Balance,
        data: Vec<u8>,
    ) -> Result<()>;

    /// Perform a batch transfer of `token_ids` to the `to` account from the `from` account.
    ///
    /// The number of `values` specified to be transferred must match the number of `token_ids`,
    /// otherwise this call will revert.
    ///
    /// Note that the call does not have to originate from the `from` account, and may originate
    /// from any account which is approved to transfer `from`'s tokens.
    #[ink(message)]
    fn safe_batch_transfer_from(
        &mut self,
        from: AccountId,
        to: AccountId,
        token_ids: Vec<TokenId>,
        values: Vec<Balance>,
        data: Vec<u8>,
    ) -> Result<()>;

    /// Query the balance of a specific token for the provided account.
    #[ink(message)]
    fn balance_of(&self, owner: AccountId, token_id: TokenId) -> Balance;

    /// Query the balances for a set of tokens for a set of accounts.
    ///
    /// E.g use this call if you want to query what Alice and Bob's balances are for Tokens ID 1 and
    /// ID 2.
    ///
    /// This will return all the balances for a given owner before moving on to the next owner. In
    /// the example above this means that the return value should look like:
    ///
    /// [Alice Balance of Token ID 1, Alice Balance of Token ID 2, Bob Balance of Token ID 1, Bob Balance of Token ID 2]
    #[ink(message)]
    fn balance_of_batch(
        &self,
        owners: Vec<AccountId>,
        token_ids: Vec<TokenId>,
    ) -> Vec<Balance>;

    /// Enable or disable a third party, known as an `operator`, to control all tokens on behalf of
    /// the caller.
    #[ink(message)]
    fn set_approval_for_all(&mut self, operator: AccountId, approved: bool)
        -> Result<()>;

    /// Query if the given `operator` is allowed to control all of `owner`'s tokens.
    #[ink(message)]
    fn is_approved_for_all(&self, owner: AccountId, operator: AccountId) -> bool;
}

/// The interface for an ERC-1155 Token Receiver contract.
///
/// The interface is defined here: <https://eips.ethereum.org/EIPS/eip-1155>.
///
/// Smart contracts which want to accept token transfers must implement this interface. By default
/// if a contract does not support this interface any transactions originating from an ERC-1155
/// compliant contract which attempt to transfer tokens directly to the contract's address must be
/// reverted.
#[ink::trait_definition]
pub trait Erc1155TokenReceiver {
    /// Handle the receipt of a single ERC-1155 token.
    ///
    /// This should be called by a compliant ERC-1155 contract if the intended recipient is a smart
    /// contract.
    ///
    /// If the smart contract implementing this interface accepts token transfers then it must
    /// return `ON_ERC_1155_RECEIVED_SELECTOR` from this function. To reject a transfer it must revert.
    ///
    /// Any callers must revert if they receive anything other than `ON_ERC_1155_RECEIVED_SELECTOR` as a return
    /// value.
    #[ink(message)]
    fn on_received(
        &mut self,
        operator: AccountId,
        from: AccountId,
        token_id: TokenId,
        value: Balance,
        data: Vec<u8>,
    ) -> Vec<u8>;

    /// Handle the receipt of multiple ERC-1155 tokens.
    ///
    /// This should be called by a compliant ERC-1155 contract if the intended recipient is a smart
    /// contract.
    ///
    /// If the smart contract implementing this interface accepts token transfers then it must
    /// return `BATCH_ON_ERC_1155_RECEIVED_SELECTOR` from this function. To reject a transfer it must revert.
    ///
    /// Any callers must revert if they receive anything other than `BATCH_ON_ERC_1155_RECEIVED_SELECTOR` as a return
    /// value.
    #[ink(message)]
    fn on_batch_received(
        &mut self,
        operator: AccountId,
        from: AccountId,
        token_ids: Vec<TokenId>,
        values: Vec<Balance>,
        data: Vec<u8>,
    ) -> Vec<u8>;
}

#[ink::contract]
mod erc1155 {
    use super::*;
    use redeemables::Redeemables;


    use ink_prelude::collections::BTreeMap;
    use ink_storage::traits::{PackedLayout, SpreadLayout};

    /// Indicate that a token transfer has occured.
    ///
    /// This must be emitted even if a zero value transfer occurs.
    #[ink(event)]
    pub struct TransferSingle {
        #[ink(topic)]
        operator: Option<AccountId>,
        #[ink(topic)]
        from: Option<AccountId>,
        #[ink(topic)]
        to: Option<AccountId>,
        token_id: TokenId,
        value: Balance,
    }

    /// Indicate that an approval event has happened.
    #[ink(event)]
    pub struct ApprovalForAll {
        #[ink(topic)]
        owner: AccountId,
        #[ink(topic)]
        operator: AccountId,
        approved: bool,
    }

    /// Indicate that a token's URI has been updated.
    #[ink(event)]
    pub struct Uri {
        value: ink_prelude::string::String,
        #[ink(topic)]
        token_id: TokenId,
    }

    /// Represents an (Owner, Operator) pair, in which the operator is allowed to spend funds on
    /// behalf of the operator.
    #[derive(
        Copy,
        Clone,
        Debug,
        Ord,
        PartialOrd,
        Eq,
        PartialEq,
        PackedLayout,
        SpreadLayout,
        scale::Encode,
        scale::Decode,
    )]
    #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
    struct Approval {
        owner: AccountId,
        operator: AccountId,
    }

    /// An ERC-1155 contract.
    #[ink(storage)]
    // #[derive(Default)]
    pub struct Contract {
        /// Tracks the balances of accounts across the different tokens that they might be holding.
        balances: BTreeMap<(AccountId, TokenId), Balance>,
        /// Which accounts (called operators) have been approved to spend funds on behalf of an owner.
        approvals: BTreeMap<Approval, ()>,
        /// A unique identifier for the tokens which have been minted (and are therefore supported)
        /// by this contract.
        token_id_nonce: TokenId,
        /// Details of the issuer
        nft_issuer: BTreeMap<AccountId, IssuerFeatures>,
        /// owner address of the contract
        contract_owner: AccountId,
        /// NFT Set Features
        nft_set: BTreeMap<u32, NFTSetFeatures>,
        owned_nft_set: BTreeMap<(AccountId, u32), u32>,
        owned_nft_set_index: BTreeMap<u32, u32>,
        owned_nft_set_count: BTreeMap<AccountId, u32>,
        /// Token Features
        token_features: BTreeMap<TokenId, TokenFeatures>,

        owned_set_tokens: BTreeMap<(u32, u32), TokenId>,
        owned_set_tokens_index: BTreeMap<TokenId, u32>,
        owned_set_tokens_count: BTreeMap<u32, u32>,
        owned_tokens_index: BTreeMap<TokenId, u32>,
        owned_tokens: BTreeMap<(AccountId, u32), TokenId>,
        owned_tokens_count: BTreeMap<AccountId, u32>,
        redeemable_contract: Lazy<Redeemables>,
    }

    impl Contract {
        /// Initialize a default instance of this ERC-1155 implementation.
        #[ink(constructor)]
        pub fn new(contract_address: AccountId) -> Self {
            let contract_owner = Self::env().caller();
            let redeemable_contract: Redeemables = FromAccountId::from_account_id(contract_address);
            let redeemable_contract_address = contract_address;
            Self {
                contract_owner,
                balances: Default::default(),
                approvals: Default::default(),
                token_id_nonce: Default::default(),
                nft_issuer: Default::default(),
                nft_set: Default::default(),
                owned_nft_set: Default::default(),
                owned_nft_set_index: Default::default(),
                owned_nft_set_count: Default::default(),
                token_features: Default::default(),
                owned_set_tokens: Default::default(),
                owned_set_tokens_index: Default::default(),
                owned_set_tokens_count: Default::default(),
                owned_tokens_index: Default::default(),
                owned_tokens: Default::default(),
                owned_tokens_count: Default::default(),
                redeemable_contract: Lazy::new(redeemable_contract),
            }
        }

        #[ink(message)]
        pub fn get_redeemable_contract(&self) -> Redeemables {
            self.redeemable_contract.clone()
        }

        #[ink(message)]
        pub fn get_redeemable_contract_address(&mut self) -> AccountId {
            let address = self.redeemable_contract.get_contract_address().clone();
            return address;
        }


        

        #[ink(message)]
        pub fn token_of_owner_by_index(&self, owner: AccountId, index: u32) -> TokenId {
            *self.owned_tokens.get(&(owner, index)).unwrap_or(&0)
        }

        #[ink(message)]
        pub fn owner_index_count(&self, owner: AccountId) -> u32 {
            return *self.owned_tokens_count.get(&owner).unwrap_or(&0)
        }





        /// Retrieves the owner of the contract        
        #[ink(message)]
        pub fn get_owner(&self) -> AccountId {
            return self.contract_owner;
        }

        /// Retrieves issuer name
        #[ink(message)]
        pub fn issuer_name(&self, to: AccountId) -> Option<String> {
            self.nft_issuer.get(&to).map(|v| v.name.clone())
        }

        /// Retrieves issuer status
        #[ink(message)]
        pub fn is_issuer(&self, to: AccountId) -> Option<bool> {
            self.nft_issuer.get(&to).map(|v| v.status.clone())
        }

        #[ink(message)]
        pub fn issuer_set_balance(&self, of: AccountId) -> u32 {
            *self.owned_nft_set_count.get(&of).unwrap_or(&0)
        }

        // Enumerate Issuer Set Balance

        #[ink(message)]
        pub fn set_of_owner_by_index(&self, owner: AccountId, index: u32) -> u32 {
            *self.owned_nft_set.get(&(owner, index)).unwrap_or(&0)
        }

        // the balance of nft tokens
        #[ink(message)]
        pub fn nft_set_balance(&self, setId: u32) -> u32 {
            *self.owned_set_tokens_count.get(&setId).unwrap_or(&0)
        }

        // the balance of nft tokens
        // todo: return the tokenid
        #[ink(message)]
        pub fn nft_by_set_index(&self, setId: u32, index: u32) -> TokenId {
            *self.owned_set_tokens.get(&(setId, index)).unwrap_or(&0)
        }

        #[ink(message)]
        pub fn get_nft_set(&self, id: u32) -> (Option<String>, Option<String>) {
            let name = self.nft_set.get(&id).map(|v| v.name.clone());
            let description = self.nft_set.get(&id).map(|v| v.description.clone());
            return (name, description);
        }


        #[ink(message)]
        pub fn get_token(
            &self,
            id: TokenId,
        ) -> (
            Option<String>,
            Option<String>,
            Option<AccountId>,
        ) {
            let name = self.token_features.get(&id).map(|v| v.name.clone());
            let image = self.token_features.get(&id).map(|v| v.image.clone());
            let issuer = self.token_features.get(&id).map(|v| v.issuer.clone());
            return (name, image, issuer);
        }

        /// Approve issuer to mint contract.
        #[ink(message)]
        pub fn set_issuer(
            &mut self,
            to: AccountId,
            name: String,
            status: bool,
        ) -> Result<()> {
            let caller = self.env().caller();
            if caller == self.contract_owner {
                self.nft_issuer.insert(to, IssuerFeatures { name, status });
                Ok(())
            } else {
                return Err(Error::NotContractOwner);
            }
        }

                // /// Creates an NFT set
                #[ink(message)]
                pub fn create_nft_set(
                    &mut self,
                    id: u32,
                    name: String,
                    description: String,
                    status: bool,
                ) -> Result<()> {
                    let caller = self.env().caller();
                    let stat = self.nft_issuer.get(&caller).map(|v| v.status.clone());
                    if stat == Some(true) {
                        let exists = self.nft_set.get(&id).map(|v| v.status.clone());
                        if exists != Some(true) {
                            self.nft_set.insert(
                                id,
                                NFTSetFeatures {
                                    name,
                                    description,
                                    status,
                                    owner: caller,
                                },
                            );
                            self.add_set_to_enumeration(caller, id);
                            Ok(())
                        } else {
                            return Err(Error::NotIssuer);
                        }
                    } else {
                        return Err(Error::NotIssuer);
                    }
                }
        
                pub fn add_set_to_enumeration(
                    &mut self,
                    to: AccountId,
                    setid: u32,
                ) -> Result<()> {
                    //add to token count bit
                    let length = self.owned_nft_set_count.get(&to).unwrap_or(&0);
                    // Adds token to max length
                    self.owned_nft_set.insert((to, *length), setid);
                    // //
                    self.owned_nft_set_index.insert(setid, *length);
                    /// first add the token then increment the count
                    // let entry = self.owned_nft_set_count.entry(to);
                    self.owned_nft_set_count.insert(to, (length + 1));
        
                    // increase_counter_of(entry);
                    // Ok(())
                    Ok(())
                }
        
                pub fn add_token_to_set_enumeration(
                    &mut self,
                    setId: u32,
                    tokenid: TokenId,
                ) -> Result<()> {
                    //add to token count bit
                    let length = self
                        .owned_set_tokens_count
                        .get(&setId)
                        .unwrap_or(&0);
                    // Adds token to max length
                    self.owned_set_tokens.insert((setId, *length), tokenid);
                    // //
                    self.owned_set_tokens_index.insert(tokenid, *length);
                    /// first add the token then increment the count
                    self.owned_set_tokens_count.insert(setId, (length + 1));
                    // Ok(())
                    Ok(())
                }
        
        /// Create the initial supply for a token.
        ///
        /// The initial supply will be provided to the caller (a.k.a the minter), and the
        /// `token_id` will be assigned by the smart contract.
        ///
        /// Note that as implemented anyone can create tokens. If you were to deploy this to a
        /// production environment you'd probably want to lock down the addresses that are allowed
        /// to create tokens.
        // #[ink(message)]
        // pub fn create(&mut self, value: Balance) -> TokenId {
        //     let caller = self.env().caller();

        //     // Given that TokenId is a `u128` the likelihood of this overflowing is pretty slim.
        //     self.token_id_nonce += 1;
        //     self.balances.insert((caller, self.token_id_nonce), value);

        //     // Emit transfer event but with mint semantics
        //     self.env().emit_event(TransferSingle {
        //         operator: Some(caller),
        //         from: None,
        //         to: if value == 0 { None } else { Some(caller) },
        //         token_id: self.token_id_nonce,
        //         value,
        //     });

        //     self.token_id_nonce
        // }

        /// Mint a `value` amount of `token_id` tokens.
        ///
        /// It is assumed that the token has already been `create`-ed. The newly minted supply will
        /// be assigned to the caller (a.k.a the minter).
        ///
        /// Note that as implemented anyone can mint tokens. If you were to deploy this to a
        /// production environment you'd probably want to lock down the addresses that are allowed
        /// to mint tokens.


        #[ink(message)]
        pub fn mint(
            &mut self,
            token_id: TokenId,
            setid: u32,
            value: Balance,
            name: String,
            image: String
        ) -> Result<()> {
            // ensure!(token_id <= self.token_id_nonce, Error::UnexistentToken);
            let caller = self.env().caller();

            let status = self.nft_set.get(&setid).map(|v| v.owner.clone());
            if status == Some(caller) {
                // require that set works
                //todo - We should only perform this if the balance of this token for the user is 0
                //todo - check that the token does not already exist
                if self.balance_of(caller, token_id) == 0 {
                    self.add_token_to_owner_enumeration(&caller, token_id)?;
                }
                // self.add_token_to(&to, id)?;
                self.add_token_to_set_enumeration(setid, token_id);
                self.token_features.insert(
                    token_id,
                    TokenFeatures {
                        name,
                        image,
                        issuer: caller,
                    },
                );
                self.balances.insert((caller, token_id), value);
                // Emit transfer event but with mint semantics
                self.env().emit_event(TransferSingle {
                    operator: Some(caller),
                    from: None,
                    to: Some(caller),
                    token_id,
                    value,
                });
            } else {
                return Err(Error::NotIssuer);
            }
            Ok(())
        }

        // todo: test the owned_tokens count 
        pub fn add_token_to_owner_enumeration(
            &mut self,
            to: &AccountId,
            id: TokenId,
        ) -> Result<()> {
            // todo: test this
            let length = *self.owned_tokens_count.get(&to).unwrap_or(&0);
            self.owned_tokens_count.insert(*to, (length + 1));
            self.owned_tokens.insert((*to, length), id);
            self.owned_tokens_index.insert(id, length);
            Ok(())
        }


        // todo: test the owned_tokens_count 
        pub fn remove_token_from_owner_enumeration(
            &mut self,
            from: &AccountId,
            id: TokenId,
        ) -> Result<()> {
            //max token

            // We have 2 as an answer
            let lastTokenIndex = self.owned_tokens_count.get(from).unwrap_or(&0) - 1;
            self.owned_tokens_count.insert(*from, lastTokenIndex);
            // we have 1 as the answer
            let tokenIndex = *self.owned_tokens_index.get(&id).unwrap_or(&0);

            //somehow test that this works
            // actual token index
            // When the token to delete is the last token, the swap operation is unnecessary
            // if statement isnt working but isnt necessary test
            if tokenIndex != lastTokenIndex {
                //tyy changing this again to &mut
                let lastTokenId = *self
                    .owned_tokens
                    .get(&(*from, lastTokenIndex))
                    .ok_or(Error::CannotFetchValue)?;

                //let lastTokenId = *self.owned_tokens.get(&(*from, lastTokenIndex)).unwrap_or(&0);
                //let lastTokenId = self.token_of_owner_by_index(*from,lastTokenIndex);
                //replace the token name
                self.owned_tokens.insert((*from, tokenIndex), lastTokenId);
                self.owned_tokens_index.insert(lastTokenId, tokenIndex);

                //.owned_tokens_index.insert(&lastTokedId,tokenIndex);

                // let status = self
                // .operator_approvals
                // .get_mut(&(caller, to))
                // .ok_or(Error::CannotFetchValue)?;
            }

            self.owned_tokens_index.remove(&id);

            self.owned_tokens.remove(&(*from, lastTokenIndex));
            Ok(())
        }


        // #[ink(message)]
        // pub fn mint(&mut self, token_id: TokenId, value: Balance) -> Result<()> {
        //     ensure!(token_id <= self.token_id_nonce, Error::UnexistentToken);

        //     let caller = self.env().caller();
        //     self.balances.insert((caller, token_id), value);

        //     // Emit transfer event but with mint semantics
        //     self.env().emit_event(TransferSingle {
        //         operator: Some(caller),
        //         from: None,
        //         to: Some(caller),
        //         token_id,
        //         value,
        //     });

        //     Ok(())
        // }

        // Helper function for performing single token transfers.
        //
        // Should not be used directly since it's missing certain checks which are important to the
        // ERC-1155 standard (it is expected that the caller has already performed these).
        fn perform_transfer(
            &mut self,
            from: AccountId,
            to: AccountId,
            token_id: TokenId,
            value: Balance,
        ) {

            self.balances
                .entry((from, token_id))
                .and_modify(|b| *b -= value);

            if(self.balance_of(from, token_id) == 0) {
                self.remove_token_from_owner_enumeration(&from, token_id);
            }
            if self.balance_of(to, token_id) == 0 {
                self.add_token_to_owner_enumeration(&to, token_id);
            }

            self.balances
                .entry((to, token_id))
                .and_modify(|b| *b += value)
                .or_insert(value);


            let caller = self.env().caller();
            self.env().emit_event(TransferSingle {
                operator: Some(caller),
                from: Some(from),
                to: Some(from),
                token_id,
                value,
            });
        }

        // Check if the address at `to` is a smart contract which accepts ERC-1155 token transfers.
        //
        // If they're a smart contract which **doesn't** accept tokens transfers this call will
        // revert. Otherwise we risk locking user funds at in that contract with no chance of
        // recovery.
        #[cfg_attr(test, allow(unused_variables))]
        fn transfer_acceptance_check(
            &mut self,
            caller: AccountId,
            from: AccountId,
            to: AccountId,
            token_id: TokenId,
            value: Balance,
            data: Vec<u8>,
        ) {
            // This is disabled during tests due to the use of `eval_contract()` not being
            // supported (tests end up panicking).
            #[cfg(not(test))]
            {
                use ink_env::call::{
                    build_call, utils::ReturnType, ExecutionInput, Selector,
                };

                // If our recipient is a smart contract we need to see if they accept or
                // reject this transfer. If they reject it we need to revert the call.
                let params = build_call::<Environment>()
                    .callee(to)
                    .gas_limit(5000)
                    .exec_input(
                        ExecutionInput::new(Selector::new(ON_ERC_1155_RECEIVED_SELECTOR))
                            .push_arg(caller)
                            .push_arg(from)
                            .push_arg(token_id)
                            .push_arg(value)
                            .push_arg(data),
                    )
                    .returns::<ReturnType<Vec<u8>>>()
                    .params();

                match ink_env::eval_contract(&params) {
                    Ok(v) => {
                        ink_env::debug_println!(
                            "Received return value \"{:?}\" from contract {:?}",
                            v,
                            from
                        );
                        assert_eq!(
                            v,
                            &ON_ERC_1155_RECEIVED_SELECTOR[..],
                            "The recipient contract at {:?} does not accept token transfers.\n
                            Expected: {:?}, Got {:?}", to, ON_ERC_1155_RECEIVED_SELECTOR, v
                        )
                    }
                    Err(e) => {
                        match e {
                            ink_env::Error::CodeNotFound
                            | ink_env::Error::NotCallable => {
                                // Our recipient wasn't a smart contract, so there's nothing more for
                                // us to do
                                ink_env::debug_println!("Recipient at {:?} from is not a smart contract ({:?})", from, e);
                            }
                            _ => {
                                // We got some sort of error from the call to our recipient smart
                                // contract, and as such we must revert this call
                                let msg = ink_prelude::format!(
                                    "Got error \"{:?}\" while trying to call {:?}",
                                    e,
                                    from
                                );
                                ink_env::debug_println!("{}", &msg);
                                panic!("{}", &msg)
                            }
                        }
                    }
                }
            }
        }
    }

    impl super::Erc1155 for Contract {
        #[ink(message)]
        fn safe_transfer_from(
            &mut self,
            from: AccountId,
            to: AccountId,
            token_id: TokenId,
            value: Balance,
            data: Vec<u8>,
        ) -> Result<()> {
            let caller = self.env().caller();
            if caller != from {
                ensure!(self.is_approved_for_all(from, caller), Error::NotApproved);
            }

            ensure!(to != AccountId::default(), Error::ZeroAddressTransfer);

            let balance = self.balance_of(from, token_id);
            ensure!(balance >= value, Error::InsufficientBalance);

            self.perform_transfer(from, to, token_id, value);
            self.transfer_acceptance_check(caller, from, to, token_id, value, data);

            Ok(())
        }

        #[ink(message)]
        fn safe_batch_transfer_from(
            &mut self,
            from: AccountId,
            to: AccountId,
            token_ids: Vec<TokenId>,
            values: Vec<Balance>,
            data: Vec<u8>,
        ) -> Result<()> {
            let caller = self.env().caller();
            if caller != from {
                ensure!(self.is_approved_for_all(from, caller), Error::NotApproved);
            }

            ensure!(to != AccountId::default(), Error::ZeroAddressTransfer);
            ensure!(!token_ids.is_empty(), Error::BatchTransferMismatch);
            ensure!(
                token_ids.len() == values.len(),
                Error::BatchTransferMismatch,
            );

            let transfers = token_ids.iter().zip(values.iter());
            for (&id, &v) in transfers.clone() {
                let balance = self.balance_of(from, id);
                ensure!(balance >= v, Error::InsufficientBalance);
            }

            for (&id, &v) in transfers {
                self.perform_transfer(from, to, id, v);
            }

            // Can use the any token ID/value here, we really just care about knowing if `to` is a
            // smart contract which accepts transfers
            self.transfer_acceptance_check(
                caller,
                from,
                to,
                token_ids[0],
                values[0],
                data,
            );

            Ok(())
        }

        #[ink(message)]
        fn balance_of(&self, owner: AccountId, token_id: TokenId) -> Balance {
            *self.balances.get(&(owner, token_id)).unwrap_or(&0)
        }

        #[ink(message)]
        fn balance_of_batch(
            &self,
            owners: Vec<AccountId>,
            token_ids: Vec<TokenId>,
        ) -> Vec<Balance> {
            let mut output = Vec::new();
            for o in &owners {
                for t in &token_ids {
                    let amount = self.balance_of(*o, *t);
                    output.push(amount);
                }
            }
            output
        }

        #[ink(message)]
        fn set_approval_for_all(
            &mut self,
            operator: AccountId,
            approved: bool,
        ) -> Result<()> {
            let caller = self.env().caller();
            ensure!(operator != caller, Error::SelfApproval);

            let approval = Approval {
                owner: caller,
                operator,
            };

            if approved {
                self.approvals.insert(approval, ());
            } else {
                self.approvals.remove(&approval);
            }

            self.env().emit_event(ApprovalForAll {
                owner: approval.owner,
                operator,
                approved,
            });

            Ok(())
        }

        #[ink(message)]
        fn is_approved_for_all(&self, owner: AccountId, operator: AccountId) -> bool {
            self.approvals.get(&Approval { owner, operator }).is_some()
        }
    }

    impl super::Erc1155TokenReceiver for Contract {
        #[ink(message, selector = "0xF23A6E61")]
        fn on_received(
            &mut self,
            _operator: AccountId,
            _from: AccountId,
            _token_id: TokenId,
            _value: Balance,
            _data: Vec<u8>,
        ) -> Vec<u8> {
            // The ERC-1155 standard dictates that if a contract does not accept token transfers
            // directly to the contract, then the contract must revert.
            //
            // This prevents a user from unintentionally transferring tokens to a smart contract
            // and getting their funds stuck without any sort of recovery mechanism.
            //
            // Note that the choice of whether or not to accept tokens is implementation specific,
            // and we've decided to not accept them in this implementation.
            unimplemented!("This smart contract does not accept token transfer.")
        }

        #[ink(message, selector = "0xBC197C81")]
        fn on_batch_received(
            &mut self,
            _operator: AccountId,
            _from: AccountId,
            _token_ids: Vec<TokenId>,
            _values: Vec<Balance>,
            _data: Vec<u8>,
        ) -> Vec<u8> {
            // The ERC-1155 standard dictates that if a contract does not accept token transfers
            // directly to the contract, then the contract must revert.
            //
            // This prevents a user from unintentionally transferring tokens to a smart contract
            // and getting their funds stuck without any sort of recovery mechanism.
            //
            // Note that the choice of whether or not to accept tokens is implementation specific,
            // and we've decided to not accept them in this implementation.
            unimplemented!("This smart contract does not accept batch token transfers.")
        }
    }

    #[cfg(test)]
    mod tests {
        /// Imports all the definitions from the outer scope so we can use them here.
        use super::*;
        use crate::Erc1155;

        use ink_lang as ink;

        #[cfg(feature = "ink-experimental-engine")]
        fn set_sender(sender: AccountId) {
            ink_env::test::set_caller::<Environment>(sender);
        }

        #[cfg(not(feature = "ink-experimental-engine"))]
        fn set_sender(sender: AccountId) {
            const WALLET: [u8; 32] = [7; 32];
            ink_env::test::push_execution_context::<Environment>(
                sender,
                WALLET.into(),
                1000000,
                1000000,
                ink_env::test::CallData::new(ink_env::call::Selector::new([0x00; 4])), /* dummy */
            );
        }

        #[cfg(feature = "ink-experimental-engine")]
        fn default_accounts() -> ink_env::test::DefaultAccounts<Environment> {
            ink_env::test::default_accounts::<Environment>()
        }

        #[cfg(not(feature = "ink-experimental-engine"))]
        fn default_accounts() -> ink_env::test::DefaultAccounts<Environment> {
            ink_env::test::default_accounts::<Environment>()
                .expect("off-chain environment should have been initialized already")
        }

        fn alice() -> AccountId {
            default_accounts().alice
        }

        fn bob() -> AccountId {
            default_accounts().bob
        }

        fn charlie() -> AccountId {
            default_accounts().charlie
        }

        fn init_contract() -> Contract {
            let mut erc = Contract::new();
            erc.balances.insert((alice(), 1), 10);
            erc.balances.insert((alice(), 2), 20);
            erc.balances.insert((bob(), 1), 10);

            erc
        }

        #[ink::test]
        fn can_get_correct_balance_of() {
            let erc = init_contract();

            assert_eq!(erc.balance_of(alice(), 1), 10);
            assert_eq!(erc.balance_of(alice(), 2), 20);
            assert_eq!(erc.balance_of(alice(), 3), 0);
            assert_eq!(erc.balance_of(bob(), 2), 0);
        }

        #[ink::test]
        fn can_get_correct_batch_balance_of() {
            let erc = init_contract();

            assert_eq!(
                erc.balance_of_batch(vec![alice()], vec![1, 2, 3]),
                vec![10, 20, 0]
            );
            assert_eq!(
                erc.balance_of_batch(vec![alice(), bob()], vec![1]),
                vec![10, 10]
            );

            assert_eq!(
                erc.balance_of_batch(vec![alice(), bob(), charlie()], vec![1, 2]),
                vec![10, 20, 10, 0, 0, 0]
            );
        }

        #[ink::test]
        fn can_send_tokens_between_accounts() {
            let mut erc = init_contract();

            assert!(erc.safe_transfer_from(alice(), bob(), 1, 5, vec![]).is_ok());
            assert_eq!(erc.balance_of(alice(), 1), 5);
            assert_eq!(erc.balance_of(bob(), 1), 15);

            assert!(erc.safe_transfer_from(alice(), bob(), 2, 5, vec![]).is_ok());
            assert_eq!(erc.balance_of(alice(), 2), 15);
            assert_eq!(erc.balance_of(bob(), 2), 5);
        }

        #[ink::test]
        fn sending_too_many_tokens_fails() {
            let mut erc = init_contract();
            let res = erc.safe_transfer_from(alice(), bob(), 1, 99, vec![]);
            assert_eq!(res.unwrap_err(), Error::InsufficientBalance);
        }

        #[ink::test]
        fn sending_tokens_to_zero_address_fails() {
            let burn: AccountId = [0; 32].into();

            let mut erc = init_contract();
            let res = erc.safe_transfer_from(alice(), burn, 1, 10, vec![]);
            assert_eq!(res.unwrap_err(), Error::ZeroAddressTransfer);
        }

        #[ink::test]
        fn can_send_batch_tokens() {
            let mut erc = init_contract();
            assert!(erc
                .safe_batch_transfer_from(alice(), bob(), vec![1, 2], vec![5, 10], vec![])
                .is_ok());

            let balances = erc.balance_of_batch(vec![alice(), bob()], vec![1, 2]);
            assert_eq!(balances, vec![5, 10, 15, 10])
        }

        #[ink::test]
        fn rejects_batch_if_lengths_dont_match() {
            let mut erc = init_contract();
            let res = erc.safe_batch_transfer_from(
                alice(),
                bob(),
                vec![1, 2, 3],
                vec![5],
                vec![],
            );
            assert_eq!(res.unwrap_err(), Error::BatchTransferMismatch);
        }

        #[ink::test]
        fn batch_transfers_fail_if_len_is_zero() {
            let mut erc = init_contract();
            let res =
                erc.safe_batch_transfer_from(alice(), bob(), vec![], vec![], vec![]);
            assert_eq!(res.unwrap_err(), Error::BatchTransferMismatch);
        }

        #[ink::test]
        fn operator_can_send_tokens() {
            let mut erc = init_contract();

            let owner = alice();
            let operator = bob();

            set_sender(owner);
            assert!(erc.set_approval_for_all(operator, true).is_ok());

            set_sender(operator);
            assert!(erc
                .safe_transfer_from(owner, charlie(), 1, 5, vec![])
                .is_ok());
            assert_eq!(erc.balance_of(alice(), 1), 5);
            assert_eq!(erc.balance_of(charlie(), 1), 5);
        }

        #[ink::test]
        fn approvals_work() {
            let mut erc = init_contract();
            let owner = alice();
            let operator = bob();
            let another_operator = charlie();

            // Note: All of these tests are from the context of the owner who is either allowing or
            // disallowing an operator to control their funds.
            set_sender(owner);
            assert!(!erc.is_approved_for_all(owner, operator));

            assert!(erc.set_approval_for_all(operator, true).is_ok());
            assert!(erc.is_approved_for_all(owner, operator));

            assert!(erc.set_approval_for_all(another_operator, true).is_ok());
            assert!(erc.is_approved_for_all(owner, another_operator));

            assert!(erc.set_approval_for_all(operator, false).is_ok());
            assert!(!erc.is_approved_for_all(owner, operator));
        }

        #[ink::test]
        fn minting_tokens_works() {
            let mut erc = Contract::new();

            set_sender(alice());
            assert_eq!(erc.create(0), 1);
            assert_eq!(erc.balance_of(alice(), 1), 0);

            assert!(erc.mint(1, 123).is_ok());
            assert_eq!(erc.balance_of(alice(), 1), 123);
        }

        #[ink::test]
        fn minting_not_allowed_for_nonexistent_tokens() {
            let mut erc = Contract::new();

            let res = erc.mint(1, 123);
            assert_eq!(res.unwrap_err(), Error::UnexistentToken);
        }
    }
}
