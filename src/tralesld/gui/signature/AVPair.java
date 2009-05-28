/**
 * 
 */
package tralesld.gui.signature;

/**
 * AVPair is a pair of strings representing an attribute (a/k/a feature) and a value.
 * 
 * @author fdk
 * 
 */
public class AVPair implements Cloneable {

	private String attribute;
	private String value;
	
	
	public AVPair(String attribute, String value) {
		this.attribute = attribute;
		this.value = value;
	}
	
	public AVPair(String pair) {

		/*
		 * attribute and value are seperated from each other by a colon ':'
		 */
		String[] tokens = pair.split(":");
		
		this.attribute = tokens[0];
		this.value = tokens[1];
	}
	
	public AVPair() {}

	
	
	/**
	 * @return A string formatted for the JGraph node output
	 */
	public String toStringHTML() {
		StringBuffer sb = new StringBuffer();
		
		sb.append(getAttribute().toUpperCase() + ": <i>" + getValue() + "</i>");
		
		return sb.toString();
	}

	
	
	public String getAttribute() {
		return attribute;
	}
	public void setAttribute(String attribute) {
		this.attribute = attribute;
	}
	public String getValue() {
		return value;
	}
	public void setValue(String value) {
		this.value = value;
	}


	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		
		sb.append(getAttribute() + ":" + getValue());
		
		return sb.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((attribute == null) ? 0 : attribute.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AVPair other = (AVPair) obj;
		if (attribute == null) {
			if (other.attribute != null)
				return false;
		} else if (!attribute.equals(other.attribute))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public Object clone() {
		AVPair tmp = null;
		try {
			tmp = (AVPair) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}

		/*
		 * primitive types are cloned correctly by Object.clone()
		 * immutable reference types (e.g String) are shared, but that's ok because they're immutable
		 * arrays of these two sorts of types are cloned correctly by the array's clone()-method.
		 * 
		 * but we have to care for the fields with other types
		 */

		/*
		 * no other fields.
		 */
		
		return tmp;
	}

}
