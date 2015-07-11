import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.locks.*;

class Male implements Runnable{
	
	Bathroom b;
	
	public Male(Bathroom ba){
		b = ba;
	}
	
	public void run(){
		for(int i=0; i < 100000; i++){
			if(Math.random() > 0.49){
				b.enterMale();
			}else{
				b.leaveMale();
			}
			System.out.println("Male -- " + b.toString());
		}
		
		while(b.count > 0) b.leaveMale();
	}
	
}

class Female implements Runnable{
	
	Bathroom b;
	
	public Female(Bathroom ba){
		b = ba;
	}
	
	public void run(){
		for(int i=0; i < 100000; i++){
			if(Math.random() > 0.49){
				b.enterFemale();
			}else{
				b.leaveFemale();
			}
			
			System.out.println("Female -- " + b.toString());
		}
		
		while(b.count > 0) b.leaveFemale();
	}
	
}

class Bathroom{
	
	int count;
	
	int maleWaiter;
	
	int femaleWaiter;
	
	int turn; // 0 -> male | 1 -> female | 2 -> indef
	
	public Bathroom(){
		maleWaiter = 0;
		femaleWaiter = 0;
		count = 0;
		turn = 2;
	}
	
	public synchronized String toString(){
		return "Turn " + turn + " --- Inside [" + count + "] --- Male Waiting [" + maleWaiter  + "] --- Female Waiting [" + femaleWaiter + "]";
	}
	
	public synchronized void enterMale(){

		while(turn == 1){
			try{
				maleWaiter++;
				System.out.println("Male waiting");
				wait();
				maleWaiter--;
			}catch(Exception e){}
		}
		
		turn = 0;
		count++;
		
	}
	
	public synchronized void enterFemale(){

		while(turn == 0){
			try{	
				femaleWaiter++;
				System.out.println("Female waiting");
				wait();
				femaleWaiter--;
			}catch(Exception e){}
		}
		
		turn = 1;
		count++;

	}
	
	public synchronized void leaveMale(){
		if(count == 0 || turn == 1) return;
		count--;
		if(count == 0){
			if(femaleWaiter > 0){
				turn = 1;
				System.out.println("Now female turn");
				notifyAll();
			} else{
				turn = 2;
			}
		}
	}
	
	public synchronized void leaveFemale(){
		if(count == 0 || turn == 0) return;
		count--;
		if(count == 0){
			if(maleWaiter > 0){
				System.out.println("Now MALE turn");
				turn = 0;
				notifyAll();
			}else{
				turn = 2;
			}
		}
	}
	
}

public class Trabalho14{
	
	public static void main(String[] args){
		
		Bathroom b = new Bathroom();
		
		Thread f = new Thread(new Female(b));
		Thread m = new Thread(new Male(b));
		
		m.start();
		f.start();
		
		try{
		
		m.join();
		f.join();
		}catch(Exception e){}
		
	}
	
}


/////////////////////////////////////////////////q

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.locks.*;

class Male implements Runnable{
	
	Bathroom b;
	
	public Male(Bathroom ba){
		b = ba;
	}
	
	public void run(){
		for(int i=0; i < 100000; i++){
			if(Math.random() > 0.49){
				b.enterMale();
			}else{
				b.leaveMale();
			}
			System.out.println("Male -- " + b.toString());
		}
		
		while(b.count > 0) b.leaveMale();
	}
	
}

class Female implements Runnable{
	
	Bathroom b;
	
	public Female(Bathroom ba){
		b = ba;
	}
	
	public void run(){
		for(int i=0; i < 100000; i++){
			if(Math.random() > 0.49){
				b.enterFemale();
			}else{
				b.leaveFemale();
			}
			
			System.out.println("Female -- " + b.toString());
		}
		
		while(b.count > 0) b.leaveFemale();
	}
	
}

class Bathroom{
	
	Lock lock;
	
	Condition condMale;
	
	Condition condFemale;
	
	int count;
	
	int maleWaiter;
	
	int femaleWaiter;
	
	int turn; // 0 -> male | 1 -> female | 2 -> indef
	
	public Bathroom(){
		lock = new ReentrantLock();
		condMale = lock.newCondition();
		condFemale = lock.newCondition();
		maleWaiter = 0;
		femaleWaiter = 0;
		count = 0;
		turn = 2;
	}
	
	public String toString(){
		lock.lock();
		try{
			return "Turn " + turn + " --- Inside [" + count + "] --- Male Waiting [" + maleWaiter  + "] --- Female Waiting [" + femaleWaiter + "]";
	
		}finally{
			lock.unlock();
		}
	}
	
	public void enterMale(){
		
		lock.lock();
		
		try{
			while(turn == 1){
				try{
					maleWaiter++;
					System.out.println("Male waiting");
					condMale.await();
					maleWaiter--;
				}catch(Exception e){}
			}
			
			turn = 0;
			count++;
			
		} finally{
			lock.unlock();
		}
		
	}
	
	public void enterFemale(){
		lock.lock();
		
		try{
			while(turn == 0){
				try{	
					femaleWaiter++;
					System.out.println("Female waiting");
					condFemale.await();
					femaleWaiter--;
				}catch(Exception e){}
			}
			
			turn = 1;
			count++;
			
		} finally{
			lock.unlock();
		}
		
	}
	
	public void leaveMale(){
		lock.lock();
		try{
			if(count == 0 || turn == 1) return;
			count--;
			if(count == 0){
				if(femaleWaiter > 0){
					turn = 1;
					System.out.println("Now female turn");
					condFemale.signalAll();
				} else{
					turn = 2;
				}
			}
		}finally{
			lock.unlock();
		}
	}
	
	public void leaveFemale(){
		lock.lock();
		try{
			if(count == 0 || turn == 0) return;
			count--;
			if(count == 0){
				if(maleWaiter > 0){
					System.out.println("Now MALE turn");
					turn = 0;
					condMale.signalAll();
				}else{
					turn = 2;
				}
			}
		}finally{
			lock.unlock();
		}
	}
	
}

public class Book96{
	
	public static void main(String[] args){
		
		Bathroom b = new Bathroom();
		
		Thread f = new Thread(new Female(b));
		Thread m = new Thread(new Male(b));
		
		m.start();
		f.start();
		
		try{
		
		m.join();
		f.join();
		}catch(Exception e){}
		
	}
	
}
