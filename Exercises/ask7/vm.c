#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdint.h>

#define HALT 0x00
#define JUMP 0x01
#define JNZ 0x02
#define DUP  0x03
#define DROP 0x04
#define PUSH4 0x05
#define PUSH2 0x06
#define PUSH1 0x07
#define ADD 0x08
#define SUB 0x09
#define MUL 0x0A
#define DIV 0x0B
#define MOD 0x0C
#define EQ 0x0D
#define NE 0x0E
#define LT 0x0F
#define GT 0x10
#define LE 0x11
#define GE 0x12
#define NOT 0x13
#define AND 0x14
#define OR 0x15
#define INPUT 0x16
#define OUTPUT 0x17
#define CLOCK 0x2A
#define CONS 0x2B
#define HD 0x2C
#define TL 0x2D

struct cons_l {
    int32_t head;
    int32_t tail;
    short mark;
    struct cons_l *next;
};
typedef struct cons_l Cell;

typedef struct {
  int32_t *array;
  size_t used;
  size_t size;
} Array;

Cell *heap;
int malloc_counter;

void initArray(Array *a, size_t initialSize) {
  a->array = (int32_t *) malloc(initialSize * sizeof(int32_t));
  a->used = 0;
  a->size = initialSize;
}

void insertArray(Array *a, int32_t element) {
  // a->used is the number of used entries, because a->array[a->used++] updates a->used only *after* the array has been accessed.
  // Therefore a->used can go up to a->size 
  if (a->used == a->size) {
    a->size *= 2;
    a->array = (int32_t *)realloc(a->array, a->size * sizeof(int32_t));
  }
  a->array[a->used++] = element;
}

void freeArray(Array *a) {
  free(a->array);
  a->array = NULL;
  a->used = a->size = 0;
}


int32_t pop(Array *a) {
    if (a->used <= 0) {
        printf("Stack is empty! Cannot pop element\n");
        return -1;
    }
    a->used--;
    return a->array[a->used];
}

//we create a new cell and add it to the heap with the others
Cell* createCell(int32_t head, int32_t tail) {
    Cell *new_cell = (Cell *) malloc(sizeof(Cell));
    malloc_counter++;
    new_cell->head=head;
    new_cell->tail=tail;
    new_cell->mark=0;
    new_cell->next=heap;
    heap=new_cell;
    return new_cell;
}

//x is the address of a cons cell. we use DFS to mark every node accesible to x if it's not marked already
void mark_phase(int32_t x) {
    if (x & 1) {
        //we made sure x is a pointer to a cons cell
        Cell *cons;
        cons= (Cell *) (x & (~1));
        if (cons->mark == 1) return;
        //if unmarked, mark the current node
        cons->mark=1;
        mark_phase(cons->head);
        mark_phase(cons->tail);
    }
    return;
}

//we delete every cell which isn't marked
void sweep_phase() {
    if (heap==NULL) return;
    Cell *prev,*curr;
    prev=heap;
    curr=heap->next;

    //we start the search from the second node
    while(curr!=NULL) {
        if (curr->mark==1) {
            curr->mark=0;
            prev=curr;
            curr=curr->next;
        }
        else {
            prev->next=curr->next;
            free(curr);
            curr=prev->next;
        }
    }
    //after we deleted the unmarked nodes in the list, we check if the first node needs to go
    //if it's unmarked we change the value of 'heap' pointer
    if (heap->mark==0) {
        Cell *temp=heap;
        heap=heap->next;
        free(temp);
    }
    return;
}

int main(int argc, char *argv[]) {
    
    Array *stack;
    int i,dont_care,flag,pc;
    clock_t start,end;
    int lower_bits,upper_bits,addr,res;
    int32_t a,b,b1,b2,b3,b4,top,elem;
    int32_t box_for_stack;
    unsigned char *program;
    char c;
    FILE *fp;

    //printf("The start\n");
    start = clock();
    if (argc != 2) {
        printf("You need to give 1 argument\n");
        return 1;
    }
    fp = fopen(argv[1],"r");
    if (fp==NULL) {
        printf("Error reading input file\n");
        exit(1);
    }
    //'program' buffer contains the entire input
    program = (unsigned char *) malloc(65536*sizeof(unsigned char));
    //printf("before read\n");
    size_t length = fread(program, 1, 65536, fp);
    
    //printf("before initArray\n");

    //Initializations
    heap = (Cell *) malloc(sizeof(Cell));
    heap = NULL;
    stack = (Array *) malloc(sizeof(Array));
    initArray(stack,16);
    malloc_counter=0;

    //printf("after allocating\n");
    pc =0;
    flag=0;
    //printf("Before while\n");
    while(pc < length) {

        if (malloc_counter==100000) {
            //time for garbage collection
            malloc_counter=0;

            //we find every cell in the stack and mark it along with every cell connected to it
            for(i=0; i<stack->used; i++) {
                int32_t el = stack->array[i];
                if (el & 1) {
                    Cell *cons = (Cell *) (el & (~1));
                    cons->mark=1;
                    mark_phase(cons->head);
                    mark_phase(cons->tail);
                }
            }
            sweep_phase();
        }

        //printf("opcode: %#x\n", program[pc]);
        switch(program[pc]) {
            case HALT:
                flag=1;
                break;
            case JUMP:
                //printf("got in jump\n");
                lower_bits =  program[pc+1];
                upper_bits =  program[pc+2];
                upper_bits = upper_bits << 8;
                addr = upper_bits  | lower_bits;
                if (addr >= length) {
                    printf("Invalid jump address\n");
                    pc+=3;
                    break;
                }
                //pc = &program[0] + addr;
                pc = addr;
                break;
            case JNZ:
                top = pop(stack);
                //printf("element popped was: %d\n",top);
                if ((top==0) || (top == -1)) {
                    pc+=3;
                    break;
                }
                lower_bits = program[pc+1];
                upper_bits = program[pc+2];
                upper_bits = upper_bits << 8;
                addr = upper_bits  | lower_bits;
                if (addr >= length) {
                    printf("Invalid jump address\n");
                    pc+=3;
                    break;
                }
                pc = addr;
                break;
            case DUP:
                i= program[pc+1];
                elem = stack->array[stack->used-i-1];
                //push(stack[elem]);
                insertArray(stack,elem);
                //printf("element pushed to stack: %d\n",elem);
                pc+=2;
                break;
            case DROP:
                dont_care=pop(stack);
                pc++;
                break;
            case PUSH4:
                elem = program[pc+1] | (program[pc+2] << 8) | (program[pc+3] << 16) | (program[pc+4] << 24);
                insertArray(stack,(elem << 1));
                //printf("element pushed to stack: %d\n",elem);
                pc+=5;
                break;
            case PUSH2:
                b1 = (int32_t) program[pc+1];
                b2 = ((int32_t) program[pc+2]) << 8;
                elem = b1 | b2;
                insertArray(stack,(elem << 1));
                //printf("element pushed to stack: %d\n",elem);
                pc+=3;
                break;
            case PUSH1:
                elem= (int32_t) program[pc+1];
                insertArray(stack,(elem << 1));
                //printf("element pushed to stack: %d\n",elem);
                pc+=2;
                break;
            case ADD:
                pc++;
                //we check if the stack has enough elements to perform the operation
                if (stack->used < 2) {
                    printf("Add failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack) >> 1;
                // if (a == -1){
                //     printf("Add failed. Not enough elements in stack\n");
                //     break;
                // }
                a = pop(stack) >> 1;
                // if (b == -1) {
                //     printf("Add failed. Not enough elements in stack\n");
                //     break;
                // }
                insertArray(stack,(a+b)<<1);
                break;
            case SUB:
                pc++;
                if (stack->used < 2) {
                    printf("Sub failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;
                a = pop(stack)>>1;
                insertArray(stack,(a-b)<<1);                
                break;
            case MUL:
                pc++;
                if (stack->used < 2) {
                    printf("Mult failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;
                a = pop(stack)>>1;               
                insertArray(stack,(a*b)<<1);                
                break;
            case DIV:
                pc++;
                if (stack->used < 2) {
                    printf("Div failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;                
                a = pop(stack)>>1;
                if (b != 0) {
                    insertArray(stack,(a/b)<<1);
                }
                else printf("Attempted to divide by 0. 2 elements of the stack lost\n");                
                break;
            case MOD:
                pc++;
                if (stack->used < 2) {
                    printf("Mod failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;                
                a = pop(stack)>>1;                
                if (b != 0) {                    
                    insertArray(stack,(a % b)<<1);
                }
                else printf("Attempted to divide by 0. 2 elements of the stack lost\n");                
                break;
            case EQ:
                pc++;
                if (stack->used < 2) {
                    printf("Equal failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;
                a = pop(stack)>>1;
                res = (a == b)<<1;
                insertArray(stack,res);
                break;
            case NE:
                pc++;
                if (stack->used < 2) {
                    printf("Not equal failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;                
                a = pop(stack)>>1;                
                res = (a != b)<<1;
                insertArray(stack,res);
                break;
            case LT:
                pc++;
                if (stack->used < 2) {
                    printf("Lower than failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;                
                a = pop(stack)>>1;               
                res = (a < b)<<1;
                insertArray(stack,res);                
                break;
            case GT:
                pc++;
                if (stack->used < 2) {
                    printf("Greater than failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;                
                a = pop(stack)>>1;
                res = (a > b)<<1;
                insertArray(stack,res);                
                break;
            case LE:
                pc++;
                if (stack->used < 2) {
                    printf("Lower or equal failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;                
                a = pop(stack)>>1;                
                res = (a <= b)<<1;
                insertArray(stack,res);                
                break;
            case GE:
                pc++;
                if (stack->used < 2) {
                    printf("Greater or equal failed. Not enough elements in stack\n");
                    break;
                }               
                b = pop(stack)>>1;
                a = pop(stack)>>1;
                res = (a >= b)<<1;
                insertArray(stack,res);            
                break;
            case NOT:
                pc++;
                if (stack->used < 1) {
                    printf("Not failed. Not enough elements in stack\n");
                    break;
                }
                a = pop(stack)>>1;
                insertArray(stack, (a == 0)<<1);                
                break;
            case AND:
                pc++;
                if (stack->used < 2) {
                    printf("AND failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;
                a = pop(stack)>>1;
                res = (a && b)<<1;
                insertArray(stack,res);
                break;
            case OR:
                pc++;
                if (stack->used < 2) {
                    printf("OR failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack)>>1;                
                a = pop(stack)>>1;                
                res = (a || b)<<1;
                insertArray(stack,res);               
                break;
            case INPUT:
                scanf("%c",&c);
                insertArray(stack, ((int32_t) c) << 1);
                pc++;
                break;
            case OUTPUT:
                top = pop(stack);            
                if (top != -1) {
                    top = top >> 1;
                    printf("%c",top);
                }
                else printf("Stack empty! Cannot pop element.\n");
                pc++;
                break;
            case CLOCK:
                end = clock();
                double time_taken = ((double) (end - start))/CLOCKS_PER_SEC;
                printf("Time passed since the beginning: %0.6lf\n", time_taken);
                pc++;
                break;
            case CONS:
                pc++;
                if (stack->used < 2) {
                    printf("CONS failed. Not enough elements in stack\n");
                    break;
                }
                b = pop(stack);
                a = pop(stack);
                //we check if the popped values are pointers or numbers
                //if they are numbers (lsb=0) they are shifted 1 bit to the right
                //if they are pointers we make the lsb=0
                if ((b & 1)==0) b>>=1;
                else b&=(~1);
                if ((a & 1)==0) a>>=1;
                else a&=(~1);
                Cell* new_cell = createCell(a,b);
                box_for_stack = ((int32_t) new_cell) | 1;
                insertArray(stack, box_for_stack);
                break;
            case HD:
                pc++;
                int32_t pop_value1 = pop(stack);
                if ((pop_value1 & 1) == 1){
                    Cell *pop_cell1 = (Cell *) (pop_value1 & (~1));
                    insertArray(stack, pop_cell1->head);
                }
                else printf("popped element wasn't a cons cell!\n"); 
                break;
            case TL:
                pc++;
                int32_t pop_value2 = pop(stack);                
                if ((pop_value2 & 1) == 1) {
                    Cell *pop_cell2 = (Cell *) (pop_value2 & (~1));
                    insertArray(stack, pop_cell2->tail);
                }
                else printf("popped element wasn't a cons cell!\n");
                break;
            default:
                printf("No instruction matched\n");
                pc++;
        }
        if (flag==1) break;
    }
    //printf("the end\n");

    freeArray(stack);
    fclose(fp);
    return 0;
}
