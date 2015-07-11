import java.util.*;
import java.util.concurrent.locks.*;
class Fichador{

	int ficha;

	Lock lock;

	public Fichador(){
		ficha = 0;
		lock = new ReentrantLock(true);
	}

	public int obterFicha(){
		lock.lock();
		try{
			return ficha++;
		}finally{
			lock.unlock();
		
	}

}


class Worker implements Runnable{

	Fichador fichador;

	public Worker(Fichador f){
		fichador = f;
	}

	public void run(){
		for(int i=0; i < 10000; i++) fichador.obterFicha();
	}

}

public class Trabalho13{

	public static void main(String[] args){

		ArrayList<Thread> threads = new ArrayList<Thread>();

		Fichador fichador = new Fichador();

		for(int i=0; i < 100; i++){
			Thread t = new Thread(new Worker(fichador));
			threads.add(t);
			t.start();
		}

		for(Thread t : threads) t.join();

	}

}


// Solução é melhor pois apresenta justiça através do uso do ReentrantLock(true)

//Safety: duas threads nao pegam a mesma ficha
// Liveness: uma thread, dado o tempo suficiente, ira obter uma ficha
