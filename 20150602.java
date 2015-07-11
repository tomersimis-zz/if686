
class Node{
	
	int val;
	Node next;
	
	public Node(int n){
		val = n;
	}
	
}

class SafeQueue{
	
	private Node head, tail;
	
	public synchronized void add(int n){
		Node newNode = new Node(n);
		
		if(head == null){
			head = newNode;
			tail = newNode;
		}else{
			tail.next = newNode;
			tail = newNode;
		}
	}
	
	public synchronized int pop() throws Exception{
		
		if(head == null) throw new Exception("List empty");
		
		int t = head.val;
		head = head.next;
		return t;
		
	}
	
	
}

public class Exercicio4{
	
		
	
}



class Node{
	
	int val;
	Node next;
	
	public Node(int n){
		val = n;
	}
	
}

class SafeQueue{

	public static boolean isWorking = false;
	
	private Node head, tail;
	
	public void add(int n){
	
		Node newNode = new Node(n);
		
		while(isWorking){}
		isWorking = true;
		
		if(head == null){
			head = newNode;
			tail = newNode;
		}else{
			tail.next = newNode;
			tail = newNode;
		}
		
		isWorking = false;
	}
	
	public int pop() throws Exception{
	
		while(isWorking){}
		
		isWorking = true;
		
		if(head == null) throw new Exception("List empty");
		
		int t = head.val;
		head = head.next;
		
		isWorking = false;
		return t;
		
	}
	
	
}

public class Exercicio5{
	
		
	
}
