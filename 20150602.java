///Trabalho 12

import java.util.*;

class Worker implements Runnable{
	
	int i0, j0, i1, j1;
	int[][] a, b, res;
	
	public Worker(int i0, int j0, int i1, int j1, int[][] a, int[][] b, int[][] res){
		System.out.println("Thread from " + i0 + "," + j0 + " to "  + i1 + "," + j1);
		this.i1 = i1;
		this.j1 = j1;
		this.i0 = i0;
		this.j0 = j0;
		this.a = a;
		this.b = b;
		this.res = res;
	}
	
	public void run(){	
		
		int els = (i1-i0)*3 + (j1-j0+1);
		
		int i=i0, j=j0;	
		
		for(int e = 0; e < els; e++){
			
			System.out.println(">> Calculating " + i + "," + j + " -- " + Thread.currentThread().getId());
				
			int sum = 0;
			
			for(int x=0; x < a[0].length; x++){
				sum += a[i][x]*b[x][j];
			}
			
			res[i][j] = sum;
			
			
			j++;
			if(j == b[0].length){
				i++;
				j = 0;
			}
		}
		
	}
	
}

public class Trabalho12{
	
	public static int[][] multiply(int[][] a, int[][] b, int n){
		
		int[][] res = new int[a.length][b[0].length];
		
		int elem = a.length*b[0].length;
		
		int elemByThread = elem/n;
		
		int i0 = 0, j0 = 0;
		
		ArrayList<Thread> threads = new ArrayList<Thread>();
		
		for(int i=1; i < n; i++){
			
			int i1 = i0, j1 = j0;
			
			for(int e = 0; e < elemByThread-1; e++){
				j1++;
				if(j1 == b[0].length){
					i1++;
					j1 = 0;
				}
			}
			
			
			Thread t = new Thread(new Worker(i0,j0,i1,j1, a,b,res));
			threads.add(t);
			t.start();
			
			i0 = i1;
			j0 = j1;

		}
		
		j0++;
		if(j0 == b[0].length){
			i0++;
			j0 = 0;
		}
		
		Thread z = new Thread(new Worker(i0,j0, a.length -1 ,b[0].length-1, a,b,res));
		threads.add(z);
		z.start();
		
		for(Thread t : threads){
			try{
				t.join();
			}catch(Exception e){}
		}
		
		return res;
		
	}
	
	
	public static void main(String[] args){
		
		long init = System.currentTimeMillis();
		
		int[][] a = {{1,2,3}, {4,5,6}, {7,8,9}};
	
		int[][] b = {{1,2,3}, {4,5,6}, {7,8,9}};
		
		int[][] res = multiply(a,b, 2);
		
		for(int i=0; i < a.length; i++){
			for(int j=0; j < b[0].length; j++){
				System.out.print(res[i][j] + " ");
			}
			System.out.println();
		}
		
		System.out.println("Total time: " + (System.currentTimeMillis() - init));
		
	}
	
	
	
}

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
