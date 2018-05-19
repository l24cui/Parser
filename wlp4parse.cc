#include <set>
#include <string>
#include <iostream>
#include <sstream>
#include <stack>
#include <vector>
#include <map>
#include <list>
#include <utility>
#include <fstream>
using namespace std;

struct Tree {
    pair<string,string> node;
    list<Tree> children;

    Tree(pair<string,string> n):node{n} {}
};

ifstream file{"wlp4tok.lr1"};
set<string> terms;
set<string> nonterms;
vector<pair<string,string>> prods;
string start;
int numTerm,numNTerm,numProd,numTrans,numState;
map<pair<int,string>,pair<string,int>> trans;
string seq;
vector<pair<string,string>> seqterms;
stack<string> inputs;
stack<int> states;
list<Tree> reduceprods;


int convert(string s) {
  int ret;
  string str;
  stringstream ss{s};
  if (ss >> ret) {
    if (!(ss >> str)) return ret;
  }
  throw string("ERROR");
}

void readsyms(set<string> &t,int &n) {
  string temp;
  if (!getline(file,temp)) {
    throw string("ERROR");
  }
  n = convert(temp);
  for(int i = 0; i < n; i++) {
    if (!getline(file,temp)) {
      throw string("ERROR");
    }
    t.insert(temp);
  }
  if (t.size() != n) {
    throw string("ERROR");
  }
}

void readrules(vector<pair<string,string>> &r) {
  string temp;
  if (!getline(file,temp)) {
    throw string("ERROR");
  }
  numProd = convert(temp);
  for(int i = 0; i < numProd; i++) {
    if (!getline(file,temp)) {
      throw string("ERROR");
    }
    stringstream ss{temp};
    string from,to;
    if (ss >> from) {
      to = temp.replace(0,from.length()+1,"");
      pair<string,string> rule (from,to);
      vector<pair<string,string>>::iterator it;
      for (it = r.begin(); it != r.end(); ++it) {
        if (*it == rule) break;
      }
      if (it == r.end()) {
        r.emplace_back(rule);
      } else {
        throw string("ERROR");
      }
    }
  }
}

void readtrans(map<pair<int,string>,pair<string,int>> &t) {
  string temp;
  if (!getline(file,temp)) {
    throw string("ERROR");
  }
  numTrans = convert(temp);
  for(int i = 0; i < numTrans; i++) {
    if (!getline(file,temp)) {
      throw string("ERROR");
    }
    stringstream ss{temp};
    int state;
    string sym;
    if ((ss >> state) && (ss >> sym) &&
        (state < numState) && (state >= 0) &&
        (terms.count(sym)+nonterms.count(sym) == 1)) {
      pair<int,string> key (state,sym);
      int state1;
      string act;
      if ((ss >> act) && (ss >> state1) &&
          (state1 < numState) && (state1 >= 0) &&
          ((act == "shift") || (act == "reduce")) &&
          (t.count(key) == 0)) {
        pair<string,int> action (act,state1);
        t[key] = action;
      } else {
        throw string("ERROR");
      }
    } else {
      throw string("ERROR");
    }
  }
}

void getTerms(string &s,vector<pair<string,string>> &st) {
  stringstream ss{s};
  string sym,lex;
  while (ss >> sym >> lex) {
    pair<string,string> seqt (sym,lex);
    st.emplace_back(seqt);
  }
}

void printTree(Tree &t) {
  cout << (t.node).first << " " << (t.node).second << endl;
  for (auto it = (t.children).begin(); it != (t.children).end(); ++it) {
    printTree(*it);
  }
}

int main(){
  try{
    string temp;
    // read through the file and initialize the lr parser 
    readsyms(terms,numTerm); // read terminals
    readsyms(nonterms,numNTerm); // read nonterminals
    if (!getline(file,start)) { // read start symbol
      throw string("ERROR");
    }
    if (nonterms.count(start) != 1) {
      throw string("ERROR");
    }
    readrules(prods); // read production rules
    if (!getline(file,temp)) {
      throw string("ERROR");
    }
    numState = convert(temp);
    readtrans(trans); // read transitions
    if (getline(cin,seq)) {
      string bof = "BOF BOF";
      getTerms(bof,seqterms);
      getTerms(seq,seqterms);
    }
    while (getline(cin,seq)) {
      getTerms(seq,seqterms);
    }
    if (seqterms.empty()) return 1;
    string eof = "EOF EOF";
    getTerms(eof,seqterms);
    states.push(0);
    pair<int,string> key (0,(seqterms.front()).first);
    int k = 0;
    for (auto it = seqterms.begin();it != seqterms.end();) {
      stringstream kss;
      kss << k;
      if (states.empty()) {
        throw "ERROR at " + kss.str();
      }
      if (trans.count(key) == 0) {
        throw "ERROR at " + kss.str();
      }
      string action = (trans.at(key)).first;
      if (action == "reduce") {
        int prodindex = (trans.at(key)).second;
        stringstream prodss{(prods.at(prodindex)).second};
        string stackstr;
        stack<string> rhs;
        while (prodss >> stackstr) {
          rhs.push(stackstr);
        }
        Tree newnode{prods.at(prodindex)};
        while (!(rhs.empty())) {
          if ((inputs.top() == rhs.top()) && !(inputs.empty()) && !(states.empty())) {
            inputs.pop();
            rhs.pop();
            states.pop();
            (newnode.children).push_front(reduceprods.back());
            reduceprods.pop_back();
          } else {
            throw "ERROR at " + kss.str();
          }
        }
        key.first = states.top();
        key.second = (prods.at(prodindex)).first;
        reduceprods.emplace_back(newnode);
      } else if (action == "shift") {
        int newstate = (trans.at(key)).second;
        states.push(newstate);
        inputs.push(key.second);
        if (key.second == it->first) {
          reduceprods.emplace_back(*it);
          ++k;
          ++it;
        }
        if (it != seqterms.end()) {
          key.first = states.top();
          key.second = it->first;
        }
      }
    }
    stringstream endss;
    endss << k;
    if (!(inputs.empty()) && (states.size() == 1) && (states.top() == 0)) throw "ERROR at " + endss.str();
    cout << "start BOF procedures EOF" << endl;
    for (auto itprint = reduceprods.begin(); itprint != reduceprods.end(); ++itprint) {
      printTree(*itprint);
    }
  } catch(const string& msg){
    cerr << msg << endl;
  }
}