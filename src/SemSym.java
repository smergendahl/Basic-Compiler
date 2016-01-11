import java.util.*;

/**
 * The Sym class defines a symbol-table entry. 
 * Each Sym contains a type (a Type).
 */
public class SemSym {
    private Type type;
	private int offset;
	private boolean isGlobal;
    
    public SemSym(Type type) {
        this.type = type;
		this.offset = 0;
		isGlobal = false;
    }
    
    public Type getType() {
        return type;
    }
    
    public String toString() {
        return type.toString() + ' ' + offset;
    }

	public int getOffset() {
		return offset;
	}

	public void setOffset(int value) {
		this.offset = value;
	}

	public void setGlobal(boolean bool) {
		this.isGlobal = bool;
	}

	public boolean isGlobal() {
		return this.isGlobal;
	}
}

/**
 * The FnSym class is a subclass of the Sym class just for functions.
 * The returnType field holds the return type and there are fields to hold
 * information about the parameters.
 */
class FnSym extends SemSym {
    // new fields
    private Type returnType;
    private int numParams;
    private List<Type> paramTypes;
	private int offset;
	private int localVarsSize;
	private int paramSize;
    
    public FnSym(Type type, int numparams, int localVarsSize) {
        super(new FnType());
        returnType = type;
        numParams = numparams;
		this.localVarsSize = localVarsSize;
		paramSize = numparams * 4;
		offset = paramSize + localVarsSize;
    }

    public void addFormals(List<Type> L) {
        paramTypes = L;
    }
    
    public Type getReturnType() {
        return returnType;
    }

    public int getNumParams() {
        return numParams;
    }

	public int getParamSize() {
		return paramSize;
	}

    public List<Type> getParamTypes() {
        return paramTypes;
    }

    public String toString() {
        // make list of formals
        String str = "";
        boolean notfirst = false;
        for (Type type : paramTypes) {
            if (notfirst)
                str += ",";
            else
                notfirst = true;
            str += type.toString();
        }

        str += "->" + returnType.toString();
        return str;
    }


	public int getOffset() {
		return offset;
	}

	public void setOffset(int value) {
		this.offset = value;
	}

	public int getLocalVarsSize() {
		return localVarsSize;
	}

	public void setLocalVarsSize(int value) {
		this.localVarsSize = value;
	}

	public void incOffset(int value) {
		this.offset += value;
	}

	public void decOffset(int value) {
		this.offset -= value;
	}

}


