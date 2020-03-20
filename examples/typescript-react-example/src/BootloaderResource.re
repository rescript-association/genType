/* NOTE: This is a spooky interface that provides no type safety. It should be
 * improved. Use with caution. */
[@bs.module "BootloaderResource"]
external read: JSResource.t('a) => 'a = "read";