enum Instruction {
    Return,
    Constant(ConstIndex),
    Add,
}
struct ConstIndex(usize);

type Value = f64;
pub struct VM {
    constants: Vec<Value>,
    instructions: Vec<Instruction>,
    ip: usize, // index into instructions
    stack: Vec<Value>,
}

pub struct VmError;

impl VM {
    pub fn add(&mut self) -> Result<(), VmError> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a + b);
        Ok(())
    }
    fn pop(&mut self) -> Result<Value, VmError> {
        let x = self.stack.pop().ok_or(VmError)?;
        Ok(x)
    }
    fn push(&mut self, x: Value) {
        self.stack.push(x);
    }
}
