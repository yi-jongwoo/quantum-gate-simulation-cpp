#include <bits/stdc++.h>
using namespace std;

typedef long double lf;
typedef complex<lf> lc;
typedef lc zm;
template<int nsiz,int msiz>
struct mm{
    zm **m;
    mm(){
        m=new zm*[nsiz];
        for(int i=0;i<nsiz;i++)
            m[i]=new zm[msiz];
    }
    mm(const mm&y){
        m=new zm*[nsiz];
        for(int i=0;i<nsiz;i++){
            m[i]=new zm[msiz];
            memcpy(m[i],y.m[i],msiz*sizeof(zm));
        }
    }
    ~mm(){
        for(int i=0;i<nsiz;i++)
            delete[]m[i];
        delete[]m;
    }
    void operator=(const mm& y){
    	for(int i=0;i<nsiz;i++)
            memcpy(m[i],y.m[i],msiz*sizeof(zm));
	}
    zm* operator[](int i){
        return m[i];
    }
    mm operator+(mm y){
        mm z=*this;
        for(int i=0;i<nsiz;i++)
            for(int j=0;j<msiz;j++)
                z[i][j]=z[i][j]+y[i][j];
        return z;
    }
    mm operator-(mm y){
        mm z=*this;
        for(int i=0;i<nsiz;i++)
            for(int j=0;j<msiz;j++)
                z[i][j]=z[i][j]-y[i][j];
        return z;
    }
    mm<msiz,nsiz> operator!(){
    	mm<msiz,nsiz> z;
    	for(int i=0;i<nsiz;i++)
    		for(int j=0;j<msiz;j++)
    			z[j][i]=conj(m[i][j]);
    	return z;
	}
    template<int lsiz>
    mm<nsiz,lsiz> operator*(mm<msiz,lsiz> y){
    	mm<nsiz,lsiz> z;
    	for(int k=0;k<msiz;k++)
    		for(int i=0;i<nsiz;i++)
    			for(int j=0;j<lsiz;j++){
    				z[i][j]+=m[i][k]*y[k][j];
				}
		return z;
	}
    mm operator*(zm y){
        mm z=*this;
        for(int i=0;i<nsiz;i++)
            for(int j=0;j<msiz;j++){
                z[i][j]=z[i][j]*y;
            }
        return z;
    }
	template<int ynsiz,int ymsiz>
	mm<nsiz*ynsiz,msiz*ymsiz> operator%(mm<ynsiz,ymsiz> y){
		mm<nsiz*ynsiz,msiz*ymsiz> z;
		for(int i=0;i<nsiz;i++)
			for(int yi=0;yi<ynsiz;yi++)
				for(int j=0;j<msiz;j++)
					for(int yj=0;yj<ymsiz;yj++)
						z[i*ynsiz+yi][j*ymsiz+yj]=m[i][j]*y[yi][yj];
		return z;
	}
};

template<int n=2>
struct ket:public mm<n,1>{
	ket(){}
	ket(const mm<n,1>&y):mm<n,1>(y){}
	zm& operator[](int x){
		return mm<n,1>::m[x][0];
	}
};

template<int n=2>
struct bra:public mm<1,n>{
	bra(){}
	bra(const mm<1,n>&y):mm<1,n>(y){}
	zm& operator[](int x){
		return mm<1,n>::m[0][x];
	}
};
template<int n=2>
struct oper:public mm<n,n>{
	oper(){}
	oper(const mm<n,n>&y):mm<n,n>(y){}
	oper operator^(int a){
        oper c=zm(1),x=*this;
        while(a){
            if(a&1){
                c=c*x;
            }
            x=x*x;
            a>>=1;
        }
        return c;
    }
    array<ket<n>,2>measure(ket<n> x){
    	oper I;for(int i=0;i<n;i++)I[i][i]=1;
    	oper P0 = (I+*this)*.5;
    	oper P1 = (I-*this)*.5;
    	return {ket(P0*x),ket(P1*x)};
	}
	array<oper<n>,2>measure(oper<n> x){
    	oper I;for(int i=0;i<n;i++)I[i][i]=1;
    	oper P0 = (I+*this)*.5;
    	oper P1 = (I-*this)*.5;
    	return {oper(P0*x*P0),oper(P1*x*P1)};
	}
};
template<int n>
string to_string(ket<n> x){
	string res="|";
	for(int i=0;i<n;i++){
		res+=to_string(x[i].real());
		string tmp=to_string(x[i].imag());
		if(tmp[0]!='-')
			res.push_back('+');
		res+=tmp;
		res.push_back('i');
		if(i!=n-1)
			res.push_back(',');
	}
	res.push_back('>');
	return res;
}
template<int n>
string to_string(bra<n> x){
	string res="<";
	for(int i=0;i<n;i++){
		res+=to_string(x[i].real());
		string tmp=to_string(x[i].imag());
		if(tmp[0]!='-')
			res.push_back('+');
		res+=tmp;
		res.push_back('i');
		if(i!=n-1)
			res.push_back(',');
	}
	res.push_back('|');
	return res;
}
template<int n>
string to_string(oper<n> x){
	string res="|";
	for(int i=0;i<n;i++){
		res.push_back('[');
		for(int j=0;j<n;j++){
			res+=to_string(x[i][j].real());
			string tmp=to_string(x[i][j].imag());
			if(tmp[0]!='-')
				res.push_back('+');
			res+=tmp;
			res.push_back('i');
			if(j!=n-1)
				res.push_back(',');
		}
		res.push_back(']');
	}
	res.push_back('|');
	return res;
}

zm scalar(mm<1,1> x){
	return x[0][0];
}

template<class T,int n,int m>
T& operator<<(T& o,mm<n,m>x){
	o<<"[some matrix]";
	return o;
}
template<class T,int n>
T& operator<<(T& o,mm<n,n>x){
	o<<to_string(oper<n>(x));
	return o;
}
template<class T,int n>
T& operator<<(T& o,mm<n,1>x){
	o<<to_string(ket<n>(x));
	return o;
}
template<class T,int n>
T& operator<<(T& o,mm<1,n>x){
	o<<to_string(bra<n>(x));
	return o;
}
template<class T>
T& operator<<(T& o,mm<1,1>x){
	o<<x[0][0].real();
	string tmp=to_string(x[0][0].imag());
	if(tmp[0]!='-')
		o<<'+';
	o<<tmp<<'i';
	return o;
}

template<int n>
oper<n> operator+(array<ket<n>,2> x){
	oper<n> res;
	for(ket<n>& y:x)
		res=res+y*!y;
	return res;
}
template<int n>
oper<n> operator+(array<oper<n>,2> x){
	oper<n> res;
	for(oper<n>& y:x)
		res=res+y;
	return res;
}

oper<2> rot(lf rad,oper<2> axis){
	oper I; I[0][0]=1; I[0][1]=0; I[1][0]=0; I[1][1]=1;
	return I*cos(rad/2)-axis*lc(0,sin(rad/2));
}

tuple<lf,lf,lf> quat2zyx(lf w,lf x,lf y,lf z){
	return{
		atan2(2*(w*x+y*z),1-2*(x*x+y*y)),
		-1.5707963267948966192313216916398L+2*atan2(sqrt(1+2*(w*y-x*z)),sqrt(1-2*(w*y-x*z))),
		atan2(2*(w*z+x*y),1-2*(y*y+z*z))
	};
}
tuple<lf,lf,lf> quat2xyz(lf w,lf x,lf y,lf z){
	return{
		atan2(2*(w*x-y*z),1-2*(x*x+y*y)),
		+1.5707963267948966192313216916398L-2*atan2(sqrt(1-2*(w*y+x*z)),sqrt(1+2*(w*y+x*z))),
		atan2(2*(w*z-x*y),1-2*(y*y+z*z))
	};
}

int main(){
	ket zero; zero[0]=1; zero[1]=0;
	ket one; one[0]=0; one[1]=1;
	oper I; I[0][0]=1; I[0][1]=0; I[1][0]=0; I[1][1]=1;
	oper X; X[0][0]=0; X[0][1]=1; X[1][0]=1; X[1][1]=0;
	oper Y; Y[0][0]=0; Y[0][1]=lc(0,-1); Y[1][0]=lc(0,1); Y[1][1]=0;
	oper Z; Z[0][0]=1; Z[0][1]=0; Z[1][0]=0; Z[1][1]=-1;
	//cout<<X*Y*Z;
	
	lf w=0,x=rand(),y=rand(),z=rand();
	lf n=sqrt(w*w+x*x+y*y+z*z);
	w/=n; x/=n; y/=n; z/=n;
	auto[t,u,v]=quat2zyx(w,x,y,z);cout<<'!'<<t<<' '<<u<<' '<<v<<endl;
	auto[tt,uu,vv]=quat2xyz(w,x,y,z);cout<<'!'<<tt<<' '<<uu<<' '<<vv<<endl;
	
	cout<<rot(tt,X)*rot(uu,Y)*rot(vv,Z) << "\n" << rot(v,Z)*rot(u,Y)*rot(t,X) << "\n" << I*w-X*lc(0,x)-Y*lc(0,y)-Z*lc(0,z);
	
	/*
	lf a=rand(),b=rand(),c=rand(),d=rand();
	lf r=sqrt(a*a+b*b+c*c+d*d);
	a/=r;b/=r;c/=r;d/=r;
	ket psi; psi[0]=lc(a,b); psi[1]=lc(c,d);
	auto x=scalar(!psi*X*psi); // = <psi|X|psi>
	auto y=scalar(!psi*Y*psi);
	auto z=scalar(!psi*Z*psi);
	cout<<x*x+y*y+z*z;
	*/
}
