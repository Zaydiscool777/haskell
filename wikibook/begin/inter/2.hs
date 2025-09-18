import qualified MyModule -- forces you to use mod.func
import qualified MyOtherModule hiding (lawfulEvil) as EEEE
-- you can still mod.func even w/o the keyword

someFunction text = 'c' : MyModule.remove_e text -- Will work, removes lower case e's
someOtherFunction text = 'c' : EEEE.remove_e text -- Will work, removes all e's