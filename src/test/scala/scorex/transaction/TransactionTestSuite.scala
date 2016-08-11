package scorex.transaction

import org.scalatest.Suites

class TransactionTestSuite extends Suites(
  new UTXPoolTests,
  new TransactionSpecification,
  new StoredStateUnitTests
)
