import org.scalatest.{Matchers, WordSpec}

/** ***********************************************************************
  * Copyright (c) Metabiota Incorporated - All Rights Reserved
  * ------------------------------------------------------------------------
  * This material is proprietary to Metabiota Incorporated. The 
  * intellectual and technical concepts contained herein are proprietary
  * to Metabiota Incorporated. Reproduction or distribution of this
  * material, in whole or in part, is strictly forbidden unless prior 
  * written permission is obtained from Metabiota Incorporated.
  * ************************************************************************/

/**
  * Created by cristian.schuszter on 2017-02-28.
  */

class HelloTest extends WordSpec with Matchers {
  "Hello.addNumbers" when {
    "adding two positive numbers" should {
      "give a positive number" in {
        (3 + 2) should be(5)
      }
    }
  }
}
