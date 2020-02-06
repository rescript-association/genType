/** hello */
[@genType]
let flat = 34;

/**
  * Returns the average of two numbers.
  *
  * @param message - A message to be signed
  * @param key - The keypair with which to sign the message
  * @returns A signed message
 */
[@genType]
let multiline = (. message, key) => message ++ string_of_int(key);