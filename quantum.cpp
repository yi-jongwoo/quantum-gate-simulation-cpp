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
		res.push_back('\n');
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

template<int qn,int n=(1<<qn)>
oper<n> graph(vector<pair<int,int>> vec){
	oper<n> res;
	for(int i=0;i<n;i++){
		int flag=0;
		for(auto[x,y]:vec)
			flag^=i>>(qn-1-x)&i>>(qn-1-y);
		res[i][i]=flag&1?-1:1;
	}
	return res;
}

template<int n,int m>
ket<n> extract(ket<m> x,vector<int> vec){
	sort(vec.begin(),vec.end());
	
}

int main(){
	ket zero; zero[0]=1; zero[1]=0;
	ket one; one[0]=0; one[1]=1;
	oper I; I[0][0]=1; I[0][1]=0; I[1][0]=0; I[1][1]=1;
	oper X; X[0][0]=0; X[0][1]=1; X[1][0]=1; X[1][1]=0;
	oper Y; Y[0][0]=0; Y[0][1]=lc(0,-1); Y[1][0]=lc(0,1); Y[1][1]=0;
	oper Z; Z[0][0]=1; Z[0][1]=0; Z[1][0]=0; Z[1][1]=-1;
	oper H = (X+Z)*sqrt(.5);
	oper<4> CX; CX[0][0]=1; CX[1][1]=1; CX[2][3]=1; CX[3][2]=1;
	oper<4> CZ; CZ[0][0]=1; CZ[1][1]=1; CZ[2][2]=1; CZ[3][3]=-1;
	ket plus; plus[0]=sqrt(.5); plus[1]=sqrt(.5);
	lf pi =acos(-1);
	
	lf phi=pi/4;
	oper grh=graph<5>({pair{0,2},pair{1,2},pair{2,3},pair{3,4}});
	oper m1=I%X%I%I%I;
	oper m2=I%I%X%I%I;
	oper t1=Z%I%I%Z%I;
	oper t2=I%I%I%X%Z;
	oper m3=I%I%I%(rot(-phi,Z)*X*rot(phi,Z))%I;
	oper t3=I%I%I%I%Z;
	
	ket psi=plus%zero;
	ket hid=psi%plus%plus%plus;
	hid=grh*hid;
	for(int i1=0;i1<2;i1++)for(int i2=0;i2<2;i2++)for(int i3=0;i3<2;i3++){
		ket post=m2.measure(m1.measure(hid)[i1])[i2];
		if(i2) post=t2*post;
		if(i1) post=t1*post;
		post=m3.measure(post)[i3];
		post=(I%I%I%I%H)*post;
		if(i3) post=t3*post;
		cout<<post<<endl;
	}
	
	/*
	oper grh=graph<4>({pair{0,2},pair{1,2},pair{2,3}});
	oper m1=I%X%I%I;
	oper m2=I%I%X%I;
	oper t1=Z%I%I%Z;
	oper t2=I%I%I%X;
	
	ket psi=plus%zero;
	ket hid=psi%plus%plus;
	hid=grh*hid;
	for(int i1=0;i1<2;i1++)for(int i2=0;i2<2;i2++){
		ket post=m2.measure(m1.measure(hid)[i1])[i2];
		if(i2) post=t2*post;
		if(i1) post=t1*post;
		cout<<post<<endl;
	}
	
	/*
	oper<4> m1=(rot(-pi,Z)*X*rot(pi,Z))%I;
	oper<4> t1=I%Z;
	
	ket psi=plus;
	ket<4> hid=psi%plus;
	hid = CZ*hid;
	for(int i=0;i<2;i++){
		ket<4> post=m1.measure(hid)[i];
		post = (I%H)*post;
		if(i) post = t1*post;
		cout<<post<<endl;
	}
	*/
}
