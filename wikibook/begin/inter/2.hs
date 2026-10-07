import qualified MyModule -- forces you to use mod.func
import qualified MyOtherModule as EEEE hiding (lawfulEvil)
-- you can still mod.func even w/o the keyword

someFunction text = 'c' : MyModule.removeE text -- Will work, removes lower case e's
someOtherFunction text = 'c' : EEEE.removeE text -- Will work, removes all e's