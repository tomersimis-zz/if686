import java.util.ArrayList;

class Worker implements Runnable {
	
	private long start, end;
	
	public Worker(long start, long end){
		this.start = start;
		this.end = end;
	}
	
	public void run(){
		
		for(long i=start; i <= end; i++){
			System.out.println(i + "");
		}
		
	}
	
}

public class Exercicio1{

	public static void main(String[] args){
		
		long max = 2000000000;
		
		int threads = 5;
		
		long step = max/threads;
		
		ArrayList<Thread> threadsList = new ArrayList<Thread>();
		
			
		for(long i=0; i < max; i += step){
			Thread t = new Thread(new Worker(i+1, i + step));
			t.start();
			threadsList.add(t);
		}
		
		for(Thread t : threadsList){
			try{
				t.join();
			}catch(Exception e){}
			
		}
		
	}

}
