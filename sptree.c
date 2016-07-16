#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "mpi.h"

enum message_type {SURVEY, ECHO, FALSE_ECHO, TOP, SOL, NO_SOL, LAST_SOL, STOP, ACK};

int rank, size;
int kids_count = 0;
int no_kids = 0;
int no_descendents = 0;
int parent = -1;
int sq_len;
int top_left_row, top_left_col;
int next = 0;
int zeros = 0;
int left, right;
int stop_sol = 0;
int last_sent = -1;

typedef struct kid_sol {
	int** matrix_sol;
	struct kid_sol* next;
} kid_sol;

kid_sol *right_sol;

void print_solution(int solution[size][size])
{
	int row, col;
	for (row = 0 ; row < size ; row++) {
		for (col = 0 ; col < size ; col++) {
			printf("%d", solution[row][col]);
			if (col < size - 1) {
				printf(" ");
			}
		}
		printf("\n");
	}
	printf("\n");
}

void print_solution_ptr(int** solution, FILE* file)
{
	int row, col;
	for (row = 0 ; row < size ; row++) {
		for (col = 0 ; col < size ; col++) {
			fprintf(file, "%d ", solution[row][col]);
		}
		fprintf(file, "\n");
	}
}

void print_route_table(int route_table[size])
{
	int row;
	for (row = 0 ; row < size ; row++)
	{
		printf("%d ", route_table[row]);
	}
	printf("\n");
}

void print_insertion(int insertion[zeros])
{
	int row;
	for (row = 0 ; row < zeros ; row++)
	{
		printf("%d ", insertion[row]);
	}
	printf("\n");
}

int number_digits(int x)
{
	if (x == 0)
	{
		return 1;
	}
	int digits = 0;
	while (x)
	{
		digits++;
		x /= 10;
	}
	return digits;
}

void read_descendents(int kids[size], int descendents[size], int rank, char *filename, int matrix[size][size])
{
	char *buffer;
	size_t len = 0;
	int current_rank, x;

	FILE *file = fopen(filename,"r");
	do {
		getline(&buffer, &len, file);
		sscanf(buffer, "%d", &current_rank);
	} while (current_rank != rank);
	buffer += number_digits(rank) + 3;
	while (sscanf(buffer, "%d", &x) != EOF)
	{
		buffer += number_digits(x) + 1;
		descendents[x] = 1;
		kids[x] = 1;
		matrix[rank][x] = 1;
		kids_count++;
		no_kids++;
		no_descendents++;
	}
}

void init_descendents(int descendents[size], int route_table[size], int possibilities[size], int kids[size], int matrix[size][size])
{
	int row, col;
	for (row = 0 ; row < size ; row++)
	{
		possibilities[row] = 0;
		descendents[row] = 0;
		kids[row] = 0;
		route_table[row] = -1;
		for (col = 0 ; col < size ; col++) {
			matrix[row][col] = 0;
		}
	}
}

void read_sudoku(int sudoku_init[size][size], char *filename)
{
	int row = 0, column, dim;
	char *line;
	FILE *file = fopen(filename, "r");
	
	fscanf(file, "%d", &sq_len);
	while(row < sq_len * sq_len)
	{
		column = 0;
		while (column < sq_len * sq_len)
		{
			fscanf(file, "%d", &sudoku_init[row][column]);
			column++;
		}
		row++;
	}
	
	fclose(file);
}

void get_zeros(int sudoku[size][size])
{
	int row, col;
	zeros = 0;

	for (row = top_left_row ; row < top_left_row + sq_len ; row++) {
		for (col = top_left_col ; col < top_left_col + sq_len ; col++) {
			if (sudoku[row][col] == 0) {
				zeros++;
			}
		}
	}
}

void get_empty(int sudoku[size][size], int insertion[zeros])
{
	int row, col, k = 0;
	int* found = (int*) calloc (size, sizeof(int));

	for (row = top_left_row ; row < top_left_row + sq_len ; row++) {
		for (col = top_left_col ; col < top_left_col + sq_len ; col++) {
			if (sudoku[row][col] != 0) {
				found[sudoku[row][col] - 1] = 1;
			}
		}
	}

	for (row = 0 ; row < size ; row++) {
		if (found[row] == 0) {
			insertion[k++] = row + 1;
		}
	}
}

int** copy_matrix(int sudoku[size][size])
{
	int row, col;

	int** copy = (int**) malloc (size * sizeof(int*));

	for (row = 0 ; row < size ; row++) {
		copy[row] = (int*) malloc (size * sizeof(int*));
		for (col = 0 ; col < size ; col++) {
			copy[row][col] = sudoku[row][col];
		}
	}

	return copy;
}

void swap(int *x, int *y)
{
	int temp = *x;
	*x = *y;
	*y = temp;
}

void merge(int insertion[zeros], int left, int middle, int right)
{
	int k = left;
	int l = middle + 1;
	int idx = 0;
	int sorted[right - left + 1];
	while (k <= middle || l <= right) {
		if (k > middle) {
		    sorted[idx++] = insertion[l++];
		    continue;
		}
		if (l > right) {
		    sorted[idx++] = insertion[k++];
		    continue;
		}
		if (insertion[k] < insertion[l]) {
		    sorted[idx++] = insertion[k++];
		} else {
		    sorted[idx++] = insertion[l++];
		}
	}
	for (idx = left ; idx <= right ; idx++) {
		insertion[idx] = sorted[idx - left];
	}
}

void sort_array(int insertion[zeros], int left, int right)
{
	if (left >= right) {
		return;
	}
	int middle = (left + right) / 2;
	sort_array(insertion, left, middle);
	sort_array(insertion, middle + 1, right);
	merge(insertion, left, middle, right);
}

void get_solution(int insertion[zeros], int sudoku[size][size], int left, int right, int solution[size][size])
{
	int row, col, k = 0;
	for (row = 0 ; row < size ; row++) {
		for (col = 0 ; col < size ; col++) {
			solution[row][col] = sudoku[row][col];
		}
	}
	sort_array(insertion, left, right);

	for (row = top_left_row ; row < top_left_row + sq_len ; row++) {
		for (col = top_left_col ; col < top_left_col + sq_len ; col++) {
			if (solution[row][col] == 0) {
				solution[row][col] = insertion[k++];
			}
		}
	}
}

int seek_and_destroy(int insertion[zeros])
{
	int index, idx2;
	for (index = zeros - 1 ; index >= 0 ; index--) {
		int minimum = size + 1;
		int pos = -1;
		int x = insertion[index];
		for (idx2 = index + 1 ; idx2 < zeros ; idx2++) {
		    if (insertion[idx2] > x && insertion[idx2] < minimum) {
			minimum = insertion[idx2];
			pos = idx2;
		    }
		}
		if (pos != -1) {
		    insertion[index] = insertion[pos];
		    insertion[pos] = x;
		    return index;
		}
	}
	return -1;
}

void add_partial_solution(int sudoku[size][size], kid_sol **solutions, int length[no_kids + 1], int position[size], int kid_no)
{
	int row, col;
	int index = position[kid_no];
	length[index]++;

	kid_sol *new = (kid_sol*) malloc (sizeof(kid_sol));
	new->matrix_sol = (int**) malloc (size * sizeof(int*));
	for (row = 0 ; row < size ; row++) {
		new->matrix_sol[row] = (int*) malloc (size * sizeof(int));
		for (col = 0 ; col < size ; col++) {
			new->matrix_sol[row][col] = sudoku[row][col];
		}
	}
	new->next = NULL;

	kid_sol *current = solutions[index];
	if (length[index] == 1) {
		solutions[index] = new;
	} else {
		while (current->next != NULL) {
			current = current->next;
		}
		current->next = new;
	}
}

int check_kids_start(int length[no_kids + 1])
{
	int row;
	for (row = 0 ; row < no_kids + 1 ; row++) {
		if (length[row] == 0) {
			return 0;
		}
	}
	return 1;
}

int check_ack(int array[no_kids])
{
	int row;
	for (row = 0 ; row < no_kids ; row++) {
		if (array[row] == 0) {
			return 0;
		}
	}
	return 1;
}

int valid_solution(int current_matrix[size][size], int** partial)
{
	int row, col, next_idx;
	for (row = 0 ; row < size ; row++) {
		for (col = 0 ; col < size ; col++) {
			if (partial[row][col]) {
				current_matrix[row][col] = partial[row][col];
			}
		}
	}

	for (row = 0 ; row < size ; row++) {
		for (col = 0 ; col < size ; col++) {
			for (next_idx = row + 1 ; next_idx < size ; next_idx++) {
				if (current_matrix[row][col] == current_matrix[next_idx][col] && current_matrix[row][col] != 0) {
					return 0;
				}
			}
			for (next_idx = col + 1 ; next_idx < size ; next_idx++) {
				if (current_matrix[row][col] == current_matrix[row][next_idx] && current_matrix[row][col] != 0) {
					return 0;
				}
			}
		}
	}
	return 1;
}

int valid_matrix(int matrix[size][size])
{
	int row, col, next_idx;
	for (row = 0 ; row < size ; row++) {
		for (col = 0 ; col < size ; col++) {
			for (next_idx = row + 1 ; next_idx < size ; next_idx++) {
				if (matrix[row][col] == matrix[next_idx][col] && matrix[row][col] != 0) {
					return 0;
				}
			}
			for (next_idx = col + 1 ; next_idx < size ; next_idx++) {
				if (matrix[row][col] == matrix[row][next_idx] && matrix[row][col] != 0) {
					return 0;
				}
			}
		}
	}
	return 1;
}

void add_solution(int solution[size][size])
{
	int row, col;

	kid_sol *new = (kid_sol*) malloc (sizeof(kid_sol));
	new->matrix_sol = (int**) malloc (size * sizeof(int*));
	for (row = 0 ; row < size ; row++) {
		new->matrix_sol[row] = (int*) malloc (size * sizeof(int));
		for (col = 0 ; col < size ; col++) {
			new->matrix_sol[row][col] = solution[row][col];
		}
	}
	new->next = NULL;

	if (right_sol == NULL) {
		right_sol = new;
	} else {
		kid_sol *current = right_sol;
		if (current == NULL) {
			current = right_sol;
		}
		while (current->next != NULL) {
			current = current->next;
		}
		current->next = new;
	}
}

void store_partial_solutions(int current_matrix[size][size], kid_sol **solutions, int length[no_kids + 1], int position[size], int sol_rank, int current, int** partial)
{
	if (current && valid_solution(current_matrix, partial) == 0) {
		return;
	}
	int row, col;
	if (stop_sol) {
		return;
	}

	int process[size][size];
	for (row = 0 ; row < size ; row++) {
		for (col = 0 ; col < size ; col++) {
			process[row][col] = current_matrix[row][col];
		}
	}

	if (current == no_kids + 1) {
		add_solution(current_matrix);
		if (rank == 0) {
			stop_sol = 1;
		}
		return;
	}
	kid_sol *current_node = solutions[current];
	if (position[sol_rank] == current) {
		while (current_node->next != NULL) {
			current_node = current_node->next;
		}
		store_partial_solutions(process, solutions, length, position, sol_rank, current + 1, current_node->matrix_sol);
	} else {

		while (current_node != NULL) {
			store_partial_solutions(process, solutions, length, position, sol_rank, current + 1, current_node->matrix_sol);
			current_node = current_node->next;
		}
	}
}

int main(int args, char **argv)
{
	int row, column, x, max_pos;
	int random = 0;
	int flag;
	
	MPI_Status status;

	MPI_Init(&args, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	int kids[size], descendents[size], temp[size], route_table[size], sudoku_init[size][size], possibilities[size];
	int matrix[size][size], temp_matrix[size][size];

	init_descendents(descendents, route_table, possibilities, kids, matrix);
	read_descendents(kids, descendents, rank, argv[1], matrix);

	if (rank == 0)
	{
		for (column = 0 ; column < size ; column++)
		{
			if (descendents[column])
			{
				MPI_Send(&random, 1, MPI_INT, column, SURVEY, MPI_COMM_WORLD);
			}
		}
	}

	while(1)
	{
		MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
		if (flag)
		{
			switch(status.MPI_TAG)
			{
				case SURVEY:
						MPI_Recv(&random, 1, MPI_INT, MPI_ANY_SOURCE, SURVEY, MPI_COMM_WORLD, &status);
						if (parent == -1) {
							parent = status.MPI_SOURCE;
							descendents[parent] = 0;
							kids[parent] = 0;
							kids_count--;
							no_kids--;
							no_descendents--;
							for (column = 0 ; column < size ; column++)
							{
								if (descendents[column]) {
									MPI_Send(&random, 1, MPI_INT, column, SURVEY, MPI_COMM_WORLD);
								}
							}
						} else {
							MPI_Send(&random, 1, MPI_INT, status.MPI_SOURCE, FALSE_ECHO, MPI_COMM_WORLD);
						}
						break;
				case ECHO:
						MPI_Recv(&temp_matrix, size * size, MPI_INT, MPI_ANY_SOURCE, ECHO, MPI_COMM_WORLD, &status);
						for (row = 0 ; row < size ; row++) {
							int found_one = 0;
							for (column = 0 ; column < size ; column++) {
								if (!found_one && temp_matrix[row][column]) {
									found_one = 1;
								}
								if (matrix[row][column] == 0) {
									matrix[row][column] = temp_matrix[row][column];
								}
							}
							if (found_one) {
								descendents[row] = 1;
								route_table[row] = status.MPI_SOURCE;
								no_descendents++;
							}
						}
						route_table[status.MPI_SOURCE] = status.MPI_SOURCE;
						kids_count--;
						break;
				case FALSE_ECHO:
						MPI_Recv(&random, 1, MPI_INT, MPI_ANY_SOURCE, FALSE_ECHO, MPI_COMM_WORLD, &status);
						descendents[status.MPI_SOURCE] = 0;
						kids[status.MPI_SOURCE] = 0;
						matrix[rank][status.MPI_SOURCE] = 0;
						kids_count--;
						no_kids--;
						no_descendents--;
						break;
			}
		}
		if (parent > -1 && kids_count == 0) {
			MPI_Send(&matrix, size * size, MPI_INT, parent, ECHO, MPI_COMM_WORLD);
			break;
		}
		if (rank == 0 && kids_count == 0) {
			break;
		}
	}

	MPI_Barrier(MPI_COMM_WORLD);

	for (row = 0 ; row < size ; row++)
	{
		if (row != rank && route_table[row] == -1)
		{
			route_table[row] = parent;
		}
	}

	read_sudoku(sudoku_init, argv[2]);

	top_left_row = rank / sq_len * sq_len;
	top_left_col = rank % sq_len * sq_len;
	
	kid_sol **solutions = (kid_sol**) malloc ((no_kids + 1) * sizeof(kid_sol*));

	int length[no_kids + 1];
	int position[size];
	int recv_ack[no_kids];
	int kids_last_sol[no_kids];

	int index = 0;
	kids[rank] = 1;
	for (row = 0 ; row < size ; row++) {
		if (kids[row]) {
			if (rank == row) {
				position[row] = no_kids;
				length[no_kids] = 0;
			} else {
				position[row] = index;
				length[index] = 0;
				recv_ack[index] = 0;
				kids_last_sol[index++] = 0;
			}
		} else {
			position[row] = -1;
		}
	}

	int sol_count = 0;
	int first = 1;
	int end = 0;
	int my_turn = 0;
	int my_end = 0;
	int kids_end = 0;
	int kids_stop = 0;
	int kids_start = 0;
	int swap_position;
	int sol_rank;
	int own_sol[size][size];
	int recv_sol[size][size];
	int process[size][size];
	int current = 0;
	int sent_ack = 0;
	int received;
	int waiting_stop = 0;

	int** poo;

	get_zeros(sudoku_init);

	left = 0;
	right = zeros;
	int insertion[zeros];

	get_empty(sudoku_init, insertion);

	if (zeros == 0) {
		my_end = 1;
	}

	if (rank == 0)
	{
		if (my_end == 1) {
			add_partial_solution(sudoku_init, solutions, length, position, rank);
			length[position[rank]] = 1;
		}
		while (1)
		{
			int x = 0;
			if (my_turn)
			{
				if (first)
				{
					get_solution(insertion, sudoku_init, 0, zeros - 1, own_sol);
					sol_rank = rank;
					first = 0;
				} else {
					swap_position = seek_and_destroy(insertion);
					if (swap_position == -1) {
						my_end = 1;
						sol_rank = -1;
					} else {
						get_solution(insertion, sudoku_init, swap_position + 1, zeros - 1, own_sol);
						sol_rank = rank;
					}
				}
				if (!my_end) {
					add_partial_solution(own_sol, solutions, length, position, rank);
				}
			} else {
				MPI_Iprobe(MPI_ANY_SOURCE, NO_SOL, MPI_COMM_WORLD, &flag, &status);
				if (flag) {
					MPI_Recv(&random, 1, MPI_INT, MPI_ANY_SOURCE, NO_SOL, MPI_COMM_WORLD, &status);
					kids_last_sol[position[status.MPI_SOURCE]] = 1;
				}

				MPI_Iprobe(MPI_ANY_SOURCE, SOL, MPI_COMM_WORLD, &flag, &status);
				if (flag) {
					MPI_Recv(&recv_sol, size * size, MPI_INT, MPI_ANY_SOURCE, SOL, MPI_COMM_WORLD, &status);
					add_partial_solution(recv_sol, solutions, length, position, status.MPI_SOURCE);
					sol_rank = status.MPI_SOURCE;
				} else {
					sol_rank = -1;
				}
			}
			if (!kids_start && check_kids_start(length)) {
				kids_start = 1;
			}
			if (kids_start && sol_rank != -1) {
				for (row = 0 ; row < size ; row++) {
					for (column = 0 ; column < size ; column++) {
						process[row][column] = sudoku_init[row][column];
					}
				}
				store_partial_solutions(process, solutions, length, position, sol_rank, current, poo);
			}
			if (stop_sol) {
				break;
			}

			if (check_ack(kids_last_sol) == 1) {
				kids_end = 1;
			}

			if (!my_end && !kids_end) {
				my_turn = 1 - my_turn;
			} else {
				if (my_end && kids_end) {
					break;
				}
				if (my_end) {
					my_turn = 0;
				}
				if (kids_end) {
					my_turn = 1;
				}
			}
		}
		for (row = 0 ; row < size ; row++) {
			if (kids[row] == 1 && row != rank) {
				MPI_Send(&random, 1, MPI_INT, row, STOP, MPI_COMM_WORLD);
			}
		}
		while (check_ack(recv_ack) == 0)
		{
			MPI_Iprobe(MPI_ANY_SOURCE, SOL, MPI_COMM_WORLD, &flag, &status);
			if (flag)
			{
				MPI_Recv(&recv_sol, size * size, MPI_INT, MPI_ANY_SOURCE, SOL, MPI_COMM_WORLD, &status);
			}
			MPI_Iprobe(MPI_ANY_SOURCE, ACK, MPI_COMM_WORLD, &flag, &status);
			if (flag)
			{
				MPI_Recv(&random, 1, MPI_INT, MPI_ANY_SOURCE, ACK, MPI_COMM_WORLD, &status);
				recv_ack[position[status.MPI_SOURCE]] = 1;
			}
		}
	}

	if (rank > 0 && no_kids > 0)
	{
		if (my_end == 1) {
			add_partial_solution(sudoku_init, solutions, length, position, rank);
			length[position[rank]] = 1;
		}
		while (1)
		{
			received = 0;
			if (my_turn)
			{
				if (first)
				{
					get_solution(insertion, sudoku_init, 0, zeros - 1, own_sol);
					sol_rank = rank;
					first = 0;
				} else {
					swap_position = seek_and_destroy(insertion);
					if (swap_position == -1) {
						my_end = 1;
						sol_rank = -1;
					} else {
						get_solution(insertion, sudoku_init, swap_position + 1, zeros - 1, own_sol);
						sol_rank = rank;
					}
				}
				if (!my_end) {
					add_partial_solution(own_sol, solutions, length, position, rank);
				}
			} else {
				MPI_Iprobe(MPI_ANY_SOURCE, SOL, MPI_COMM_WORLD, &flag, &status);
				if (flag) {
					MPI_Recv(&recv_sol, size * size, MPI_INT, MPI_ANY_SOURCE, SOL, MPI_COMM_WORLD, &status);
					add_partial_solution(recv_sol, solutions, length, position, status.MPI_SOURCE);
					sol_rank = status.MPI_SOURCE;
					received = 1;
				} else {
					sol_rank = -1;
					MPI_Iprobe(MPI_ANY_SOURCE, NO_SOL, MPI_COMM_WORLD, &flag, &status);
					if (flag) {
						MPI_Recv(&random, 1, MPI_INT, MPI_ANY_SOURCE, NO_SOL, MPI_COMM_WORLD, &status);
						kids_last_sol[position[status.MPI_SOURCE]] = 1;
					}
				}
			}

			if (!kids_start && check_kids_start(length)) {
				kids_start = 1;
			}
			if (kids_start && sol_rank != -1) {
				for (row = 0 ; row < size ; row++) {
					for (column = 0 ; column < size ; column++) {
						process[row][column] = sudoku_init[row][column];
					}
				}
				store_partial_solutions(process, solutions, length, position, sol_rank, current, poo);
			}
			if (kids_start) {
				kid_sol *crt = right_sol;
				int crt_sol = -1;
				while (crt_sol < last_sent) {
					crt_sol++;
					crt = crt->next;
				}
				while (crt != NULL) {
					for (row = 0 ; row < size ; row++) {
						for (column = 0 ; column < size ; column++) {
							process[row][column] = crt->matrix_sol[row][column];
						}
					}
					MPI_Send(&process, size * size, MPI_INT, parent, SOL, MPI_COMM_WORLD);
					last_sent++;
					crt = crt->next;
				}
			}

			MPI_Iprobe(parent, STOP, MPI_COMM_WORLD, &flag, &status);
			if (flag) {
				MPI_Recv(&random, 1, MPI_INT, parent, STOP, MPI_COMM_WORLD, &status);
				break;
			}

			if (!kids_end && check_ack(kids_last_sol) == 1) {
				kids_end = 1;
			}

			if (!my_end && !kids_end) {
				my_turn = 1 - my_turn;
			} else {
				if (my_end && kids_end) {
					MPI_Send(&random, 1, MPI_INT, parent, NO_SOL, MPI_COMM_WORLD);
					waiting_stop = 1;
					break;
				}
				if (my_end) {
					my_turn = 0;
				}
				if (kids_end) {
					my_turn = 1;
				}
			}
		}
		if (waiting_stop) {
			MPI_Recv(&random, 1, MPI_INT, parent, STOP, MPI_COMM_WORLD, &status);
		}
		for (row = 0 ; row < size ; row++) {
			if (kids[row] == 1 && row != rank) {
				MPI_Send(&random, 1, MPI_INT, row, STOP, MPI_COMM_WORLD);
			}
		}
		while (check_ack(recv_ack) == 0)
		{
			MPI_Iprobe(MPI_ANY_SOURCE, SOL, MPI_COMM_WORLD, &flag, &status);
			if (flag)
			{
				MPI_Recv(&recv_sol, size * size, MPI_INT, MPI_ANY_SOURCE, SOL, MPI_COMM_WORLD, &status);
			}
			MPI_Iprobe(MPI_ANY_SOURCE, ACK, MPI_COMM_WORLD, &flag, &status);
			if (flag)
			{
				MPI_Recv(&random, 1, MPI_INT, MPI_ANY_SOURCE, ACK, MPI_COMM_WORLD, &status);
				recv_ack[position[status.MPI_SOURCE]] = 1;
			}
		}
		MPI_Send(&random, 1, MPI_INT, parent, ACK, MPI_COMM_WORLD);
	}

	if (rank > 0 && no_kids == 0)
	{
		if (my_end) {
			MPI_Send(&sudoku_init, size * size, MPI_INT, parent, SOL, MPI_COMM_WORLD);
			MPI_Send(&random, 1, MPI_INT, parent, NO_SOL, MPI_COMM_WORLD);
		}
		while (1)
		{
			if (!my_end)
			{
				if (first)
				{
					get_solution(insertion, sudoku_init, 0, zeros - 1, own_sol);
					first = 0;
				} else {
					swap_position = seek_and_destroy(insertion);
					if (swap_position == -1) {
						my_end = 1;
						MPI_Send(&random, 1, MPI_INT, parent, NO_SOL, MPI_COMM_WORLD);
					} else {
						get_solution(insertion, sudoku_init, swap_position + 1, zeros - 1, own_sol);
					}
				}
				if (!my_end) {
					if (valid_matrix(own_sol) == 1) {
						MPI_Send(&own_sol, size * size, MPI_INT, parent, SOL, MPI_COMM_WORLD);
					}
				}
			}
			MPI_Iprobe(parent, STOP, MPI_COMM_WORLD, &flag, &status);
			if (flag) {
				MPI_Recv(&random, 1, MPI_INT, parent, STOP, MPI_COMM_WORLD, &status);
				break;
			}
		}
		MPI_Send(&random, 1, MPI_INT, parent, ACK, MPI_COMM_WORLD);
	}

	MPI_Barrier(MPI_COMM_WORLD);

	if (rank == 0) {
		for (row = 0 ; row < size ; row++) {
			for (column = 0 ; column < size ; column++) {
				printf("%d", matrix[row][column]);
				if (column < size - 1) {
					printf(" ");
				}
			}
			printf("\n");
		}
	}

	for (row = 0 ; row < size ; row++) {
		MPI_Barrier(MPI_COMM_WORLD);
		if (row == rank) {
			printf("rank: %d\n", rank);
			for (column = 0 ; column < size ; column++) {
				printf("%d ", route_table[column]);
			}
			printf("\n");
		}
	}

	if (rank == 0) {
		FILE *output = fopen(argv[3], "w");
		fprintf(output, "%d\n", sq_len);
		kid_sol *crt = right_sol;
		if (crt != NULL) {
			print_solution_ptr(crt->matrix_sol, output);
		}
		fclose(output);
	}

	MPI_Finalize();
}
