import java.util.*;

class Node{
	
	int val;
	Node left, right;
	
	public Node(int n){
		val = n;
	}
	
	public void print(){
		
		if(left != null) left.print();
		
		System.out.println(val);
		
		Exercicio6.count++;
		
		if(right != null) right.print();
	}
	
}

class Tree{
	
	Node root;
	
	public void add(int n){
		
		Node newNode = new Node(n);
		
		if(root == null){
			root = newNode;
		}else{
			Node parent = root;
			boolean found = false;
			while(!found){
				if(parent.val > n){
					if(parent.right == null){
						synchronized(parent){
							if(parent.right != null) continue;
							parent.right = newNode;
							found = true;
						}
					}else{
						parent = parent.right;
					}
				}else{
					if(parent.left == null){
						synchronized(parent){
							if(parent.left != null) continue;
							parent.left = newNode;
							found = true;
						}
					}else{
						parent = parent.left;
					}
				}
			}
		}
		
	}
	
}

class Worker implements Runnable{
	
	Tree t;

	
	public Worker(Tree t){
		this.t = t;
	}
	
	public void run(){
		
		Random rand = new Random();
		
		for(int i=0; i < 2000; i++){
			t.add(rand.nextInt(1000000));
		}
		
	}
	
}

public class Exercicio6{
	
	public static int count = 0;
	
	
	public static void main(String[] args){
		
		ArrayList<Thread> threads = new ArrayList<Thread>();
		
		Tree t = new Tree();
		
		for(int i=0; i < 50; i++){
			threads.add(new Thread(new Worker(t)));
			threads.get(i).start();
		}
		
		for(Thread td : threads){
			try{
				td.join();
			}catch(Exception e){}
		}
		
		t.root.print();
		
		System.out.println(" >>> " + count);
		
	}

}


import java.util.*;
import java.util.concurrent.locks.*;

class Node{

	int val;
	Node left, right;

	Lock lockLeft;

	Lock lockRight;

	boolean isDummy;

	public Node(int n){
		isDummy = false;
		val = n;
		lockLeft = new ReentrantLock();
		lockRight = new ReentrantLock();
	}

	public void print(){

		if(left != null) left.print();

		if(!isDummy){
			System.out.println(val);
			Exercicio7.count++;
		}


		if(right != null) right.print();
	}

}

class Tree{

	Node root;

	public Tree(){
		root = new Node(Integer.MAX_VALUE);
		root.isDummy = true;
	}

	public void add(int n){

		Node newNode = new Node(n);

		Node parent = root;

		boolean found = false;

		while(!found){
			if(parent.val > n){
				if(parent.right == null){
					parent.lockRight.lock();
					try{
						if(parent.right != null) continue;
						parent.right = newNode;
						found = true;
					}finally{
						parent.lockRight.unlock();
					}
				}else{
					parent = parent.right;
				}
			}else{
				if(parent.left == null){
					parent.lockLeft.lock();
					try{
						if(parent.left != null) continue;
						parent.left = newNode;
						found = true;
					}finally{
						parent.lockLeft.unlock();
					}
				}else{
					parent = parent.left;
				}
			}
		}
	}



}

class Worker implements Runnable{

	Tree t;


	public Worker(Tree t){
		this.t = t;
	}

	public void run(){

		Random rand = new Random();

		for(int i=0; i < 2000; i++){
			t.add(rand.nextInt(1000000));
		}

	}

}

public class Exercicio7{

	public static int count = 0;


	public static void main(String[] args){

		ArrayList<Thread> threads = new ArrayList<Thread>();

		Tree t = new Tree();

		for(int i=0; i < 50; i++){
			threads.add(new Thread(new Worker(t)));
			threads.get(i).start();
		}

		for(Thread td : threads){
			try{
				td.join();
			}catch(Exception e){}
		}

		t.root.print();

		System.out.println(" >>> " + count);

	}

}
